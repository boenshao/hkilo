import Control.Exception (bracket)
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits ((.&.))
import Data.Char
import Data.List
import Data.Maybe
import Data.Time.Clock
import Safe
import System.Environment
import System.Exit
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Terminal
import Text.Read

main :: IO ()
main =
  setupRawMode $ do
    args <- getArgs
    st <- editorInit $ headMay args
    evalStateT editorLoop st

data Editor = Editor
  { screenW :: !Int
  , screenH :: !Int
  , cursorX :: !Int
  , cursorY :: !Int
  , colOffset :: !Int
  , rowOffset :: !Int
  , renderX :: !Int
  , dirty :: !Int
  , -- TODO: mutable buffers
    rows :: ![String]
  , render :: ![String]
  , filename :: !(Maybe String)
  , message :: !String
  , messageTime :: UTCTime
  , quitCount :: Int
  }

type EditorM = StateT Editor IO

getScreenSize :: IO (Int, Int)
getScreenSize = do
  putStr "\x1b[999C\x1b[999B"
  wh <- getCursorPosition
  pure $ case wh of
    Just (w', h') -> (w', h')
    Nothing -> (80, 24)

readUntil :: Char -> Int -> IO String
readUntil stopChar maxLen = go 0 []
 where
  go n acc = do
    c <- getChar
    let n' = n + 1
        acc' = c : acc
    if c == stopChar || n' >= maxLen
      then pure (reverse acc')
      else go n' acc'

getCursorPosition :: IO (Maybe (Int, Int))
getCursorPosition = do
  putStr "\x1b[6n"
  hFlush stdout
  s <- readUntil 'R' 32
  pure $ do
    s0 <- stripPrefix "\x1b[" s
    let (rowStr, rest) = break (== ';') s0
    s1 <- stripPrefix ";" rest
    let (colStr, _) = break (== 'R') s1
    row <- readMaybe rowStr
    col <- readMaybe colStr
    pure (col, row)

readLines :: String -> IO [String]
readLines f = do
  content <- readFile f
  pure $ lines content

tabW :: Int
tabW = 8

renderRow :: String -> String
renderRow s = go s 0
 where
  go s' i = case s' of
    '\t' : rest -> concat (replicate r " ") ++ go rest (i + r)
    c : rest -> c : go rest i'
    _ -> ""
   where
    i' = i + 1
    r = tabW - (i' `rem` tabW)

quitTime :: Int
quitTime = 2

editorInit :: Maybe String -> IO Editor
editorInit f = do
  (w, h) <- getScreenSize
  rows' <- case f of
    Just s -> readLines s
    Nothing -> pure []
  now <- getCurrentTime
  pure
    Editor
      { screenW = w
      , screenH = h - 2 -- leave 2 lines for status bar
      , cursorX = 0
      , cursorY = 0
      , colOffset = 0
      , rowOffset = 0
      , renderX = 0
      , dirty = 0
      , rows = rows'
      , render = map renderRow rows'
      , filename = f
      , message = "HELP: Ctrl-S = save | Ctrl-Q = quit"
      , messageTime = now
      , quitCount = quitTime
      }

editorPutStr :: String -> EditorM ()
editorPutStr = liftIO . putStr

editorGetChar :: EditorM Char
editorGetChar = liftIO getChar

drawRows :: EditorM ()
drawRows = do
  editorPutStr "\x1b[?25l" -- hide cursor
  editorPutStr "\x1b[H" -- put cursor top-left
  go 0
 where
  go n = do
    w <- gets screenW
    h <- gets screenH
    dx <- gets colOffset
    dy <- gets rowOffset
    render' <- gets render
    let i = n + dy
    let l = length render'
    if i >= l
      then do
        editorPutStr "~"
        when (null render' && n == (h `div` 3)) $ do
          let welcome = "hkilo editor " ++ " " ++ show w ++ "x" ++ show h
          let padding = (w - length welcome) `div` 2
          editorPutStr (concat $ replicate (padding - 1) " ")
          editorPutStr (take (w - 1) welcome)
      else
        editorPutStr (take w $ drop dx $ render' !! i)
    editorPutStr "\x1b[K"
    editorPutStr "\r\n"
    when (n < h - 1) $ go (n + 1)

drawCursor :: EditorM ()
drawCursor = do
  rx <- gets renderX
  cy <- gets cursorY
  dx <- gets colOffset
  dy <- gets rowOffset
  editorPutStr $ -- terminal is 1-indexed
    "\x1b[" ++ show (cy - dy + 1) ++ ";" ++ show (rx - dx + 1) ++ "H"
  editorPutStr "\x1b[?25h" -- show cursor

drawStatusBar :: EditorM ()
drawStatusBar = do
  w <- gets screenW
  f <- gets filename
  cx <- gets cursorX
  cy <- gets cursorY
  dx <- gets colOffset
  dy <- gets rowOffset
  rx <- gets renderX
  dirty' <- gets dirty
  rows' <- gets rows
  let f' = fromMaybe "[No Name]" f ++ if dirty' > 0 then "*" else ""
  let my = length rows'
  let rs = take w (" " ++ "dx:" ++ show dx ++ ",dy:" ++ show dy ++ ",rx:" ++ show rx ++ ",cx:" ++ show cx ++ ",cy:" ++ show cy ++ ",total:" ++ show my)
  let ls = take (w - length rs) $ f' ++ concat (replicate (w - length f') " ")
  editorPutStr "\x1b[7m"
  editorPutStr (ls ++ rs)
  editorPutStr "\x1b[m"
  editorPutStr "\r\n"

setStatusMessage :: String -> EditorM ()
setStatusMessage s = do
  now <- liftIO getCurrentTime
  modify' $ \st -> st{message = s, messageTime = now}

drawStatusMessage :: EditorM ()
drawStatusMessage = do
  w <- gets screenW
  m <- gets message
  mt <- gets messageTime
  now <- liftIO getCurrentTime
  editorPutStr "\x1b[K"
  when (diffUTCTime now mt < 5) $ editorPutStr (take w m)

moveCursor :: EditorKey -> EditorM ()
moveCursor key = do
  h <- gets screenH
  cx <- gets cursorX
  cy <- gets cursorY
  dy <- gets rowOffset
  rows' <- gets rows
  let my = length rows'
  let mx = if cy < my then length (rows' !! cy) else 0
  case key of
    ArrowLeft ->
      if cx /= 0
        then modify' $ \st -> st{cursorX = cx - 1}
        else when (cy > 0) $
          modify' $
            \st -> st{cursorX = length (rows' !! (cy - 1)), cursorY = cy - 1}
    ArrowRight ->
      if cx /= mx
        then modify' $ \st -> st{cursorX = cx + 1}
        else modify' $ \st -> st{cursorX = 0, cursorY = min my (cy + 1)}
    ArrowUp -> modify' $ \st -> st{cursorY = max 0 (cy - 1)}
    ArrowDown -> modify' $ \st -> st{cursorY = min my (cy + 1)}
    PageUp -> modify' $ \st -> st{cursorY = max 0 (dy - h)}
    PageDown -> modify' $ \st -> st{cursorY = min my (dy + h + h - 1)}
    Home -> modify' $ \st -> st{cursorX = 0}
    End -> modify' $ \st -> st{cursorX = mx}
    _ -> pure ()
  cx' <- gets cursorX
  cy' <- gets cursorY
  let mx' = if cy' < my then length (rows' !! cy') else 0
  modify' $ \st -> st{cursorX = min mx' cx'}

toRx :: String -> Int
toRx row = go row 0
 where
  go s rx =
    case s of
      '\t' : rest -> go rest (rx + (tabW - ((rx + 1) `rem` tabW)))
      _ : rest -> go rest (rx + 1)
      _ -> rx

scrollScreen :: EditorM ()
scrollScreen = do
  w <- gets screenW
  h <- gets screenH
  cx <- gets cursorX
  cy <- gets cursorY
  dx <- gets colOffset
  dy <- gets rowOffset
  rows' <- gets rows
  modify' $ \st -> st{renderX = toRx $ take cx (rows' !! cy)}
  rx <- gets renderX
  when (rx < dx) $ modify' $ \st -> st{colOffset = rx}
  when (rx >= dx + w) $ modify' $ \st -> st{colOffset = rx - w + 1}
  when (cy < dy) $ modify' $ \st -> st{rowOffset = cy}
  when (cy >= dy + h) $ modify' $ \st -> st{rowOffset = cy - h + 1}

insertChar :: String -> Int -> Char -> String
insertChar s i c = front ++ c : back where (front, back) = splitAt i s

editorInsertChar :: Char -> EditorM ()
editorInsertChar c = do
  cx <- gets cursorX
  cy <- gets cursorY
  rows' <- gets rows
  dirty' <- gets dirty
  let rows'' = case splitAt cy rows' of
        (front, s : back) -> front ++ insertChar s cx c : back
        (front, []) -> front ++ [[c]]
  modify' $ \st ->
    st
      { rows = rows''
      , render = map renderRow rows''
      , cursorX = cx + 1
      , dirty = dirty' + 1
      }

editorInderNewLine :: EditorM ()
editorInderNewLine = do
  cx <- gets cursorX
  cy <- gets cursorY
  rows' <- gets rows
  dirty' <- gets dirty
  let rows'' = case splitAt cy rows' of
        (front, s : back) -> front ++ s1 : s2 : back where (s1, s2) = splitAt cx s
        (front, []) -> front ++ [""]
  modify' $ \st ->
    st
      { rows = rows''
      , render = map renderRow rows''
      , cursorX = 0
      , cursorY = cy + 1
      , dirty = dirty' + 1
      }

deleteChar :: String -> Int -> String
deleteChar s i = front ++ back where (front, _ : back) = splitAt i s

editorDeleteChar :: EditorM ()
editorDeleteChar = do
  cx <- gets cursorX
  cy <- gets cursorY
  rows' <- gets rows
  dirty' <- gets dirty
  let my = length rows'
  if cy == my || cx == 0 && cy == 0
    then pure ()
    else
      if cx > 0
        -- TODO: a better pattern match
        then do
          let rows'' = case splitAt cy rows' of
                (front, s : back) -> front ++ deleteChar s (cx - 1) : back
                (front, []) -> front
          modify' $ \st ->
            st
              { rows = rows''
              , render = map renderRow rows''
              , cursorX = cx - 1
              , dirty = dirty' + 1
              }
        else do
          let rows'' = case splitAt (cy - 1) rows' of
                (front, s1 : s2 : back) -> front ++ (s1 ++ s2) : back
                (front, s1 : back) -> front ++ s1 : back
                (front, []) -> front
          modify' $ \st ->
            st
              { rows = rows''
              , render = map renderRow rows''
              , cursorX = length $ rows' !! (cy - 1)
              , cursorY = cy - 1
              , dirty = dirty' + 1
              }

rowsToString :: [String] -> String
rowsToString rs =
  case rs of
    s : rest -> s ++ "\n" ++ rowsToString rest
    _ -> ""

safeInit :: String -> String
safeInit s = if null s then s else init s

editorPrompt :: String -> EditorM String
editorPrompt s = go s ""
 where
  go s' r = do
    setStatusMessage (s' ++ r)
    scrollScreen
    drawRows
    drawStatusBar
    drawStatusMessage
    drawCursor
    key <- readKey
    case key of
      Backspace -> go s' (safeInit r)
      Delete -> go s' (safeInit r)
      Literal b
        | not (isControl b) && (ord b < 128) -> go s' (r ++ [b])
        | b == '\r' -> do
            setStatusMessage ""
            pure r
        | b == ctrlKey 'h' -> go s' (safeInit r)
        | otherwise -> go s' r
      _ -> go s' r

editorSave :: EditorM ()
editorSave = do
  rows' <- gets rows
  f <- gets filename
  f' <- case f of
    Just s -> pure s
    Nothing -> editorPrompt "Save as: "
  if null f'
    then
      setStatusMessage "Save aborted"
    else do
      let fileContents = rowsToString rows'
      -- TODO: error handling
      liftIO $ writeFile f' fileContents
      modify' $ \st -> st{dirty = 0, filename = Just f'}
      setStatusMessage $ show (length fileContents) ++ " bytes written to disk"

data EditorKey
  = Backspace
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | PageUp
  | PageDown
  | Home
  | End
  | Delete
  | Literal Char

ctrlKey :: Char -> Char
ctrlKey c = chr $ ord c .&. 0x1f

readKey :: EditorM EditorKey
readKey = do
  c <- editorGetChar
  if c /= '\x1b'
    then pure $ case ord c of
      127 -> Backspace
      _ -> Literal c
    else do
      c0 <- editorGetChar
      case c0 of
        '[' -> do
          c1 <- editorGetChar
          case c1 of
            b | isDigit b -> do
              c2 <- editorGetChar
              pure $
                if c2 /= '~'
                  then Literal c
                  else case c1 of
                    '1' -> Home
                    '3' -> Delete
                    '4' -> End
                    '5' -> PageUp
                    '6' -> PageDown
                    '7' -> Home
                    '8' -> End
                    _ -> Literal c
            _ ->
              pure $ case c1 of
                'A' -> ArrowUp
                'B' -> ArrowDown
                'C' -> ArrowRight
                'D' -> ArrowLeft
                'H' -> Home
                'F' -> End
                _ -> Literal c
        'O' -> do
          c1 <- editorGetChar
          pure $ case c1 of
            'H' -> Home
            'F' -> End
            _ -> Literal c
        _ ->
          pure (Literal c)

editorLoop :: EditorM ()
editorLoop = do
  scrollScreen
  drawRows
  drawStatusBar
  drawStatusMessage
  drawCursor

  dirty' <- gets dirty
  q <- gets quitCount
  key <- readKey
  case key of
    Backspace -> editorDeleteChar
    ArrowLeft -> moveCursor key
    ArrowRight -> moveCursor key
    ArrowUp -> moveCursor key
    ArrowDown -> moveCursor key
    PageUp -> moveCursor key
    PageDown -> moveCursor key
    Home -> moveCursor key
    End -> moveCursor key
    Delete -> moveCursor ArrowRight >> editorDeleteChar
    Literal b
      | b == ctrlKey 'q' ->
          if dirty' > 0 && q > 0
            then do
              setStatusMessage ("WARNING!!! File has unsaved changes. Press Ctrl-Q " ++ show q ++ " more times to quit.")
              modify' $ \st -> st{quitCount = q - 1}
              editorLoop
            else
              editorPutStr "\x1b[2J" -- clear screen
                >> editorPutStr "\x1b[H" -- put cursor top-left
                >> liftIO exitSuccess
      | b == ctrlKey 's' -> editorSave
      | b == ctrlKey 'h' -> pure ()
      | b == ctrlKey 'l' -> pure ()
      | b == '\x1b' -> pure ()
      | b == '\r' -> editorInderNewLine
      | otherwise -> editorInsertChar b

  modify' $ \st -> st{quitCount = quitTime}
  editorLoop

setupRawMode :: IO a -> IO a
setupRawMode action = do
  old <- getTerminalAttributes stdInput
  let raw =
        foldl
          withoutMode
          old
          [ EnableEcho -- ECHO
          , ProcessInput -- ICANON
          , KeyboardInterrupts -- ISIG
          , StartStopOutput -- IXON
          , ExtendedFunctions -- IEXTEN
          , MapCRtoLF -- ICRNL
          , ProcessOutput -- OPOST
          , InterruptOnBreak -- BRKINT
          , CheckParity -- INPCK
          , StripHighBit -- ISTRIP
          ]
      raw' =
        withMinInput (withTime raw 0) 1 -- VMIN=1, VTIME=0
  bracket
    ( do
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdin False
        setTerminalAttributes stdInput raw' Immediately
    )
    ( \_ -> do
        setTerminalAttributes stdInput old Immediately
        hSetEcho stdin True
        hSetBuffering stdin LineBuffering
        hSetBuffering stdout LineBuffering
    )
    (const action)
