module Main where

import Graphics.Gloss 
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game

startX, startY, bottomY, leftX, shiftX, shiftY, fScale :: Float 
startX = 250 
startY = 250 
bottomY = 19 
leftX = 17 
shiftX = 30
shiftY = 24
fScale = 1 

data State = State
  { _x :: Float
  , _y :: Float
  , _by :: Float 
  , _lx :: Float
  , _sx :: Float
  , _sy :: Float
  , _md :: Int              -- input mode (0:normal 1:insert 2:insert2)
  , _tc :: Char             -- temporary stored character
  , _tx :: String
  , hime :: Picture
  , osds :: [Maybe Picture]
  }

converter :: Char -> Int
converter st = case st of
     'あ' -> 0; 'い' -> 1; 'う' -> 2; 'え' -> 3; 'お' -> 4
     'か' -> 15; 'き' -> 19; 'く' -> 23; 'け' -> 27; 'こ' -> 31
     'さ' -> 55; 'し' -> 51; 'す' -> 46; 'せ' -> 42; 'そ' -> 37
     'た' -> 53; 'ち' -> 49; 'つ' -> 44; 'て' -> 40; 'と' -> 35
     'な' -> 17; 'に' -> 21; 'ぬ' -> 25; 'ね' -> 29; 'の' -> 33
     'は' -> 16; 'ひ' -> 20; 'ふ' -> 24; 'へ' -> 28; 'ほ' -> 32
     'ま' -> 18; 'み' -> 22; 'む' -> 26; 'め' -> 30; 'も' -> 34
     'や' -> 56; 'ゐ' -> 52; 'ゆ' -> 47; 'ゑ' -> 43; 'よ' -> 38
     'ら' -> 54; 'り' -> 50; 'る' -> 45; 'れ' -> 41; 'ろ' -> 36
     'わ' -> 57; 'を' -> 39; 'ん' -> 48; ' ' -> 14;
     'が' -> (-15); 'ぎ' -> (-19); 'ぐ' -> (-23); 'げ' -> (-27); 'ご' -> (-31)
     'ざ' -> (-55); 'じ' -> (-51); 'ず' -> (-46); 'ぜ' -> (-42); 'ぞ' -> (-37)
     'だ' -> (-53); 'ぢ' -> (-49); 'づ' -> (-44); 'で' -> (-40); 'ど' -> (-35)
     'ば' -> (-16); 'び' -> (-20); 'ぶ' -> (-24); 'べ' -> (-28); 'ぼ' -> (-32)
     _    -> 99 

window :: Display
window = InWindow "は" (640,640) (100,0)

testText :: String
testText = "あきのたの かりほのいほの とまをあらみわがころもでは つゆにぬれつつ\nきみがよは ちよにやちよに さざれいしのいはをとなりて こけのむすまで\n\nあかはなま いきひにみうく"

initState :: State
initState = State {_x=0, _y=0, _by=bottomY, _lx=leftX, _sx=0, _sy=0
                  , _md=0, _tc=' ', _tx=testText, hime=blank, osds=[Just blank]}

drawPic :: State -> Picture
drawPic st = 
  let himeB = translate (startX+40) (startY+30) $ scale 2 2 $ hime st
      numList = map (map converter) $ lines $ _tx st
      line = lineStartX (_by st) numList
      picOsds = makePicOsds numList line st
      picDaku = makePicDaku numList line st
      cursor = translate (startX-shiftX*_x st) (startY-shiftY*_y st+shiftY/2) $
                  color orange $ rectangleSolid 28 2
   in pictures $ cursor:himeB:picOsds++picDaku

lineStartX :: Float -> [[a]] -> [Float]
lineStartX by list = foldl (\acc ls -> acc++[(last acc +
              (if (length ls>0) then fromIntegral
                    (ceiling $ (fromIntegral (length ls))/(by+1))
                                else 1))]) [0] list 

makePicOsds :: [[Int]] -> [Float] -> State -> [Picture]
makePicOsds numList line st = 
  let txOsds = map (\ls -> map ((!!) (osds st)) (map abs ls)) numList
   in concat $ zipWith (\txos ln -> zipWith (\(Just t) (x,y) ->
        translate (startX-shiftX*x) (startY-shiftY*y) $ scale fScale fScale $ t)
                      txos [(a,b)|a<-[ln..],b<-[0..(_by st)]]) txOsds line

makePicDaku :: [[Int]] -> [Float] -> State -> [Picture]
makePicDaku numList line st =
  let Just daku = osds st !! 58
   in concat $ zipWith (\nml ln -> zipWith (\t (x,y) -> 
        if(t<0) then translate (startX-shiftX*x) (startY-shiftY*y) $ scale fScale fScale $ daku
                else blank
                        ) nml [(a,b)|a<-[ln..],b<-[0..(_by st)]]) numList line

updateState :: Event -> State -> State
updateState (EventKey key ks mdf _) st = keyEvent key ks mdf st
updateState _                       st = st

nextPic :: Float -> State -> State
nextPic _ st = st

up, down, right, left :: State -> State
up    st = if(_y st > 0) then st { _y = _y st - 1} 
                         else if (_x st>0) then st { _x = _x st - 1, _y = _by st}
                                           else st { _x = 0, _y = 0 }
down  st = if(_y st < _by st+1) then st { _y = _y st + 1} else st { _x = _x st + 1, _y = 1 }
right st = if(_x st > 0) then st { _x = _x st - 1} else st
left  st = if(_x st < _lx st) then st { _x = _x st + 1} else st

modeC :: Char -> State -> State
modeC ch st = st {_md = 2, _tc = ch}

mkTxt :: Char -> State -> State
mkTxt v st = case (_tc st) of
  'k' -> case v of
        'a' -> addTxt 'か' st; 'i' -> addTxt 'き' st; 'u' -> addTxt 'く' st
        'e' -> addTxt 'け' st; 'o' -> addTxt 'こ' st
        _   -> st
  'h' -> case v of
        'a' -> addTxt 'は' st; 'i' -> addTxt 'ひ' st; 'u' -> addTxt 'ふ' st
        'e' -> addTxt 'へ' st; 'o' -> addTxt 'ほ' st
        _   -> st
  'n' -> case v of
        'a' -> addTxt 'な' st; 'i' -> addTxt 'に' st; 'u' -> addTxt 'ぬ' st
        'e' -> addTxt 'ね' st; 'o' -> addTxt 'の' st; 'n' -> addTxt 'ん' st
        _   -> st
  'm' -> case v of
        'a' -> addTxt 'ま' st; 'i' -> addTxt 'み' st; 'u' -> addTxt 'む' st
        'e' -> addTxt 'め' st; 'o' -> addTxt 'も' st
        _   -> st
  't' -> case v of
        'a' -> addTxt 'た' st; 'i' -> addTxt 'ち' st; 'u' -> addTxt 'つ' st
        'e' -> addTxt 'て' st; 'o' -> addTxt 'と' st
        _   -> st
  'l' -> case v of
        'a' -> addTxt 'ら' st; 'i' -> addTxt 'り' st; 'u' -> addTxt 'る' st
        'e' -> addTxt 'れ' st; 'o' -> addTxt 'ろ' st
        _   -> st
  's' -> case v of
        'a' -> addTxt 'さ' st; 'i' -> addTxt 'し' st; 'u' -> addTxt 'す' st
        'e' -> addTxt 'せ' st; 'o' -> addTxt 'そ' st
        _   -> st
  'y' -> case v of
        'a' -> addTxt 'や' st; 'i' -> addTxt 'ゐ' st; 'u' -> addTxt 'ゆ' st
        'e' -> addTxt 'ゑ' st; 'o' -> addTxt 'よ' st
        _   -> st
  'w' -> case v of
        'a' -> addTxt 'わ' st; 'i' -> addTxt 'ゐ' st; 'u' -> addTxt 'う' st
        'e' -> addTxt 'ゑ' st; 'o' -> addTxt 'を' st
        _   -> st
  'g' -> case v of
        'a' -> addTxt 'が' st; 'i' -> addTxt 'ぎ' st; 'u' -> addTxt 'ぐ' st
        'e' -> addTxt 'げ' st; 'o' -> addTxt 'ご' st
        _   -> st
  'b' -> case v of
        'a' -> addTxt 'ば' st; 'i' -> addTxt 'び' st; 'u' -> addTxt 'ぶ' st
        'e' -> addTxt 'べ' st; 'o' -> addTxt 'ぼ' st
        _   -> st
  'd' -> case v of
        'a' -> addTxt 'だ' st; 'i' -> addTxt 'ぢ' st; 'u' -> addTxt 'づ' st
        'e' -> addTxt 'で' st; 'o' -> addTxt 'ど' st
        _   -> st
  'z' -> case v of
        'a' -> addTxt 'ざ' st; 'i' -> addTxt 'じ' st; 'u' -> addTxt 'ず' st
        'e' -> addTxt 'ぜ' st; 'o' -> addTxt 'ぞ' st
        _   -> st
  _   -> st

keyEvent :: Key -> KeyState -> Modifiers -> State -> State
keyEvent key ks mdf st 
  | alt mdf == Down && _md st==1 && ks==Down =
      case key of
        Char 'i' -> st {_md = 0}
        _        -> st
  | _md st==0 && ks==Down =  
      case key of
        Char 'j' -> down  st; Char 'k' -> up    st
        Char 'h' -> left  st; Char 'l' -> right st
        Char 'i' -> st {_md = 1}
        Char 'e' -> evalTxt st
        Char 'x' -> delTxt st
        _        -> st
  | _md st==1 && ks==Down =
      case key of
        Char 'a' -> addTxt 'あ' st; Char 'i' -> addTxt 'い' st
        Char 'u' -> addTxt 'う' st; Char 'e' -> addTxt 'え' st
        Char 'o' -> addTxt 'お' st; 
        Char 'k' -> modeC 'k' st; Char 'h' -> modeC 'h' st; Char 'f' -> modeC 'h' st
        Char 'n' -> modeC 'n' st; Char 'm' -> modeC 'm' st; Char 't' -> modeC 't' st
        Char 'r' -> modeC 'l' st; Char 'l' -> modeC 'l' st; Char 's' -> modeC 's' st
        Char 'y' -> modeC 'y' st; Char 'w' -> modeC 'w' st; Char 'g' -> modeC 'g' st
        Char 'b' -> modeC 'b' st; Char 'd' -> modeC 'd' st; Char 'z' -> modeC 'z' st
        Char '\b' -> delTxt st
        Char '\^O' -> st {_md = 0}
        SpecialKey KeyEnter -> addTxt '\n' st
        SpecialKey KeySpace -> addTxt ' ' st
        SpecialKey KeyBackspace -> delTxt st
        SpecialKey KeyDelete -> delTxt st
        _        -> st
  | _md st==2 && ks==Down =
      case key of
        Char 'a' -> mkTxt 'a' st; Char 'i' -> mkTxt 'i' st
        Char 'u' -> mkTxt 'u' st; Char 'e' -> mkTxt 'e' st
        Char 'o' -> mkTxt 'o' st; Char 'n' -> mkTxt 'n' st
        Char '\^O' -> st {_md = 0}
        SpecialKey KeyEnter -> addTxt '\n' st
        SpecialKey KeySpace -> addTxt ' ' st
        SpecialKey KeyBackspace -> delTxt st
        _        -> st
  | otherwise = st

addTxt :: Char -> State -> State
addTxt ch st = 
  let txs = lines (_tx st) 
      lns = lineStartX (_by st) txs 
      (lnInfo,txPos) = curToTxt st
      lnNum = fst lnInfo
      newTxs = if lnNum>(-1)
        then
          let ln = txs !! lnNum
              ltNum = fst txPos
              sp = take (snd txPos) (repeat ' ')
              newLn = (take ltNum ln)++sp++[ch]++(drop ltNum ln) 
           in (take lnNum txs)++[newLn]++(drop (lnNum+1) txs)
        else
          let blankLines = take (round (_x st - (last lns))) (repeat [])
              sp = take (round (_y st)) (repeat ' ')
           in txs++blankLines++[sp++[ch]]
      (nx,ny)= if (ch=='\n') then (_x st+1,0)
                             else if (_y st)>(_by st)
                                    then (_x st+1,1) else (_x st,_y st+1)
   in st {_tx = unlines newTxs, _x = nx, _y = ny, _md=1}

delTxt :: State -> State
delTxt st =
  let txs = lines (_tx st) 
      (lnInfo,txPos) = curToTxt st
      lnNum = fst lnInfo
      newTxs = if lnNum>(-1)
        then
          let ln = txs !! lnNum
              ltNum = fst txPos
              newLn = if (snd txPos)==0
                         then (take (ltNum-1) ln)++(drop ltNum ln) 
                         else ln
           in (take lnNum txs)++[newLn]++(drop (lnNum+1) txs)
        else txs
      (nx,ny)= if (_y st)>1
                  then (_x st,_y st-1)
                  else if (_x st>0)
                          then (_x st-1,_by st+1) else (0,0)
   in st {_tx = unlines newTxs, _x = nx, _y = ny}

evalTxt :: State -> State
evalTxt st =
  let txs = lines (_tx st) 
      (lnInfo,txPos) = curToTxt st
      lnNum = fst lnInfo
      newTx = if lnNum>(-1)
        then
          let ln = txs !! lnNum
              ltNum = fst txPos
              newLn = (take ltNum ln) 
           in unlines $ (take lnNum txs)++[newLn]
        else
           _tx st 
   in (foldl (\acc x -> addTxt x acc) st (eval newTx)) {_md = 0}

curToTxt :: State -> ((Int,(Float,Float)),(Int,Int))
curToTxt st =
  let txs = lines (_tx st) 
      lns = lineStartX (_by st) txs 
      lnInfo = getLn 0 (_x st) lns
      lnNum = fst lnInfo
  in  if lnNum>(-1)
        then
          let ln = txs !! lnNum
              txPos = getPosition (_by st) (_x st) (_y st)
                  (fst$snd lnInfo) (snd$snd lnInfo) ln 
           in (lnInfo,txPos) 
        else (lnInfo,(0,0)) 



getLn :: Int -> Float -> [Float] -> (Int,(Float,Float))
getLn _ _ (y:[]) = ((-1),(y,y+1))
getLn i x (y:z:ys) =
  if (x>=y && x<z) then (i,(y,z))
                   else getLn (i+1) x (z:ys)

getPosition :: Float -> Float -> Float -> Float -> Float -> [a] -> (Int,Int)
getPosition by x y x0 x1 ls =
  let lastLetters = fromIntegral (length ls) - (by+1)*(x-x0) 
   in if (x==(x1-1)) && lastLetters<y
          then (length ls,round (y-lastLetters))
          else (round ((by+1)*(x-x0)+y),0)


getHa :: String -> String
getHa st =
  let wds = words st
   in unwords $ snd $ foldr (\x acc ->
     if snd acc==[] && x=="は"
       then (True,[]) else if fst acc==True
              then if x=="は" then (False,snd acc) else (True,x:snd acc)
              else (False,snd acc)
                            ) (False,[]) wds 

evalInt :: String -> Int
evalInt st =
  let wds = words st
   in foldl (\acc x -> acc + osdToInt x) 0 wds
  where osdToInt wd =
          read $ map (\c -> case c of
                 'ひ' -> '1'; 'ふ' -> '2'; 'み' -> '3'; 'よ' -> '4'; 'ゐ' -> '5'
                 'む' -> '6'; 'な' -> '7'; 'や' -> '8'; 'こ' -> '9'; 'ろ' -> '0'
                 'ん' -> '-'; _ -> ' '
                     ) wd 
                            
eval :: String -> String
eval st =
  let tgt = getHa st
      tp = typeHa tgt
   in case tp of
        "int" -> intToOsd $ show (evalInt tgt)
        _     -> tgt

typeHa :: String -> String
typeHa _ = "int"

intToOsd :: String -> String
intToOsd st =
  map (\ch -> case ch of
                '1' -> 'ひ'; '2' -> 'ふ'; '3' -> 'み'; '4' -> 'よ'; '5' -> 'ゐ'
                '6' -> 'む'; '7' -> 'な'; '8' -> 'や'; '9' -> 'こ'; '0' -> 'ろ'
                '-' -> 'ん'; _ -> ' '
      ) st



  
loadPictures :: IO (Picture,[Maybe Picture])
loadPictures = do
  Just hime <- loadJuicy "img/hime.png"
  osds <- mapM (\x -> do y <- x;return y) (loadOsds 58)
  return (hime,osds) 

loadOsds :: Int -> [IO (Maybe Picture)]
loadOsds 0 = [loadJuicy "img/osd/osd0.png"]
loadOsds i = loadOsds (i-1)++[(loadJuicy ("img/osd/osd"++(show i)++".png"))]

main :: IO ()
main = do
  (hime',osds') <- loadPictures
  let st' = initState {hime = hime', osds = osds'}
  play window black 24 st' drawPic updateState nextPic
