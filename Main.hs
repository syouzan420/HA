module Main where

import Graphics.Gloss 
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import Data.Ratio

type CurX = Int; type CurY = Int; type BtY = Int; type LineNum = Int
type PosInLine = Int; type SpAftLine = Int
type Position = (PosInLine,SpAftLine)
type Lineinfo = (LineNum,(Int,Int))
type Fname = String; type Fbody = String; type Ftype = String

startX, startY, fScale :: Float
startX = 250 
startY = 250 
fScale = 1 

bottomY, leftX, shiftX, shiftY :: Int 
bottomY = 19 
leftX = 17 
shiftX = 30
shiftY = 24

data State = State
  { _x :: Int             -- cursor X
  , _y :: Int             -- cursor Y
  , _by :: Int            -- bottom Y 
  , _lx :: Int            -- left X
  , _sx :: Int            -- scroll X
  , _md :: Int              -- input mode (0:normal 1:insert 2:insert2)
  , _em :: Bool          -- true: evaluate in Arabic Numerals, false: evaluate in Oside
  , _tc :: Char             -- temporary stored character
  , _tx :: String           -- text
  , _fn :: [(Fname,Fbody)]  -- functions (name, body)
  , _info :: String 
  , _mes :: String
  , _key :: Key            
  , _ks :: KeyState
  , _mdf :: Modifiers
  , _cfk :: Int             -- count for key
  , hime :: Picture
  , osds :: [Maybe Picture]
  }

window :: Display
window = InWindow "は" (640,640) (100,0)

testText :: String
testText = "あきのたの かりほのいほの とまをあらみわがころもでは つゆにぬれつつ\nきみがよは ちよにやちよに さざれいしのいはをとなりて こけのむすまで\n\nあかはなま いきひにみうく"

initState :: State
initState = State {_x=0, _y=0, _by=bottomY, _lx=leftX, _sx=0, _md=0, _em=False 
                  , _tc=' ', _tx=testText, _fn=[], _info = "", _mes = "" 
                  , _key = Char ' ', _ks = Up, _mdf = Modifiers{shift=Up,ctrl=Up,alt=Up}
                  ,_cfk = 0, hime=blank, osds=[Just blank]}

drawPic :: State -> Picture
drawPic st = 
  let himeB = translate (startX+40) (startY+30) $ scale 2 2 $ hime st
      numList = map (map converter) $ lines $ _tx st
      line = lineStartX (_by st) numList
      picOsds = makePicOsds numList line st
      picDaku = makePicDaku numList line st
      status = translate (startX- fromIntegral (shiftX*leftX))
                         (startY- fromIntegral (shiftY*(bottomY+3))) $
                  color green $ scale 0.2 0.2 $ text (_info st)
      message = translate (startX- fromIntegral (shiftX*leftX))
                          (startY- fromIntegral (shiftY*(bottomY+3)-22)) $
                  color white $ scale 0.1 0.1 $ text (_mes st)
      cursor = translate (startX- fromIntegral (shiftX*_x st))
                         (startY- (fromIntegral (shiftY*_y st))+ (fromIntegral shiftY)/2) $
                  color orange $ rectangleSolid 28 2
   in pictures $ cursor:status:message:himeB:picOsds++picDaku

lineStartX :: BtY -> [[a]] -> [LineNum]
lineStartX by list = foldl (\acc ls -> acc++[(last acc +
              (if (length ls>0) then (ceiling $ (fromIntegral (length ls))/(fromIntegral (by+1)))
                                else 1))]) [0] list 

makePicOsds :: [[Int]] -> [LineNum] -> State -> [Picture]
makePicOsds numList line st = 
  let txOsds = map (\ls -> map ((!!) (osds st)) (map abs ls)) numList
   in concat $ zipWith (\txos ln -> zipWith (\(Just t) (x,y) ->
        let shX = fromIntegral (shiftX*x)
            shY = fromIntegral (shiftY*y)
        in translate (startX-shX) (startY-shY) $ scale fScale fScale $ t)
                      txos [(a,b)|a<-[(ln-_sx st)..],b<-[0..(_by st)]]) txOsds line

makePicDaku :: [[Int]] -> [LineNum] -> State -> [Picture]
makePicDaku numList line st =
  let Just daku = osds st !! 58
   in concat $ zipWith (\nml ln -> zipWith (\t (x,y) -> 
        let shX = fromIntegral (shiftX*x)
            shY = fromIntegral (shiftY*y)
        in if(t<0) then translate (startX-shX) (startY-shY) $ scale fScale fScale $ daku
                   else blank
                        ) nml [(a,b)|a<-[(ln-_sx st)..],b<-[0..(_by st)]]) numList line

updateState :: Event -> State -> State
updateState (EventKey key ks mdf _) st = keyEvent key ks mdf st{_key=key,_ks=ks,_cfk=0}
updateState _                       st = st

nextPic :: Float -> State -> State
nextPic _ st = 
  let newCfk = if _ks st==Down
                  then if (_cfk st>5)
                          then (-1)
                          else if (_cfk st>=0) then(_cfk st + 1) else _cfk st
                  else 0
      mode = if(_md st==0) then "normal" else "insert"
      keyn = keyName (_key st)
      evmd = if(_em st==True) then "T" else "F"
      st' = st{_cfk=newCfk, _info="mode: "++mode++"  key: "++keyn++"  evmode: "++evmd}
  in if(_cfk st<0)
        then keyEvent (_key st) (_ks st) (_mdf st) st'
        else st'
          where keyName (Char k) = [k] 
                keyName (SpecialKey k) = show k
                keyName _ = []
         
up, down, right, left :: State -> State
up    st = if(_y st > 0) then st { _y = _y st - 1} 
                         else if (_x st>0) then st { _x = _x st - 1, _y = _by st}
                                           else st { _x = 0, _y = 0 }
down  st = if(_y st < _by st+1) then st { _y = _y st + 1} else st { _x = _x st + 1, _y = 1 }
right st = if(_x st > 0) then st { _x = _x st - 1} else st {_sx = if(_sx st>0) then _sx st - 1 else 0}
left  st = if(_x st < _lx st) then st { _x = _x st + 1} else st { _sx = _sx st +1 }

modeC :: Char -> State -> State
modeC ch st = st {_md = 2, _tc = ch}

kanas :: String
kanas = "あいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゐゆゑよらりるれろがぎぐげござじずぜぞだぢづでどばびぶべぼわをん 0123456789"

osdNum :: [Int]
osdNum = [0,1,2,3,4,15,19,23,27,31,55,51,46,42,37,53,49,44,40,35,17,21,25,29,33,16,20,24,28,32,18,22,26,30,34,56,52,47,43,38,54,50,45,41,36,57,39,48,14,60,61,62,63,64,65,66,67,68,69] 

getIndex :: (Eq a) => a -> [a] -> Int
getIndex = getIndex' 0 where
  getIndex' _ _ [] = (-1)
  getIndex' i el (x:xs)
    | el==x = i
    | otherwise = getIndex' (i+1) el xs

converter :: Char -> Int 
converter ch  
  | elem ch kanas =
      case (getIndex ch kanas) of
         x  | x>44 && x<60 -> (-(osdNum !! (x-40)))
            | x>59 && x<65 -> (-(osdNum !! (x-35)))
            | x>64         -> osdNum !! (x-20)
            | otherwise    -> osdNum !! x
  | otherwise    = 14

varToNum :: Char -> Int
varToNum ch 
  | elem ch "aiueon" = case (getIndex ch "aiueon") of x -> x
  | otherwise        = (-1)

mkTxt :: Char -> State -> State
mkTxt v st = addTxt ch st
  where nv = varToNum v
        ch
          |(_tc st) == 'w'              = case nv of
                           0 -> 'わ'; 1 -> 'ゐ'; 2 -> 'う'; 3 -> 'ゑ'; 4 -> 'を'; (-1) -> 'ぷ'
          |(_tc st) == 'n' && nv == 5   = 'ん'
          |elem (_tc st) "kstnhmylgzdb" = case (getIndex (_tc st) "kstnhmylgzdb") of
                                            x -> if (nv<5) then kanas !! (5+5*x+nv) else 'ぷ'
          |otherwise                    = 'ぷ'

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
        Char 'm' -> if _em st == True then st {_em = False} else st {_em = True}
        Char 'e' -> evalTxt st
        Char 'x' -> delTxt st
        _        -> st
  | _md st==1 && ks==Down =
      case key of 
        Char x
          | x `elem` "aiueo" -> case (getIndex x "aiueo") of 
                                            y -> addTxt ("あいうえお" !! y) st
          | x `elem` "0123456789" -> case (getIndex x "0123456789") of
                                            y -> addTxt ("0123456789" !! y) st
          | x `elem` "khfnmtrlsywgbdz" -> case (getIndex x "khfnmtrlsywgbdz") of
                                            y -> modeC ("khhnmtllsywgbdz" !! y) st
        Char '\b' -> delTxt st
        Char '\^O' -> st {_md = 0}
        SpecialKey KeyEnter -> addTxt '\n' st
        SpecialKey KeySpace -> addTxt ' ' st
        SpecialKey KeyBackspace -> delTxt st
        SpecialKey KeyDelete -> delTxt st
        _        -> st
  | _md st==2 && ks==Down =
      case key of
        Char x
          | x `elem` "aiueon" -> mkTxt x st
        Char '\^O' -> st {_md = 0}
        SpecialKey KeyEnter -> addTxt '\n' st
        SpecialKey KeySpace -> addTxt ' ' st
        SpecialKey KeyBackspace -> delTxt st
        _        -> st
  | otherwise = st

addTxt :: Char -> State -> State
addTxt 'ぷ' st = st
addTxt ch st   = 
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
          let blankLines = take (_x st + _sx st - (last lns)) (repeat [])
              sp = take (_y st) (repeat ' ')
           in txs++blankLines++[sp++[ch]]
      (nx,ny,nsx)= if (ch=='\n') then if (_x st<_lx st) then (_x st+1,0,_sx st)
                                                        else (_x st,0,_sx st+1)
                             else if (_y st)>(_by st)
                                    then if (_x st<_lx st) then (_x st+1,1,_sx st) 
                                                           else (_x st,1,_sx st+1)
                                    else (_x st,_y st+1,_sx st)
           in st {_tx = unlines newTxs, _x = nx, _y = ny, _sx = nsx, _md=1}

delTxt :: State -> State
delTxt st =
  let (x',y',sx') = if (_y st)==0
                       then if (_x st>0)
                               then (_x st-1,_by st+1,_sx st)
                               else if (_sx st>0) then (0,_by st+1,_sx st-1)
                                                  else (0,0,0)
                       else (_x st,_y st,_sx st)
      st' = st{_x=x', _y=y', _sx=sx'}
      txs = lines (_tx st') 
      (lnInfo,txPos) = curToTxt st'
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
      (nx,ny,nsx)= if _x st'==0 && _y st'==0 && _sx st'==0
                      then (0,0,0) else (_x st',_y st'-1,_sx st')
  in st' {_tx = unlines newTxs, _x = nx, _y = ny, _sx = nsx}

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
                ++(if (elem 'た' (drop ltNum ln))
                    then snd $ foldl (\acc x -> case () of
                          _ | fst acc && x=='た' -> (False,snd acc++[x])
                            | fst acc            -> (True,snd acc++[x])
                          otherwise              -> (False,snd acc)
                                     ) (True,[]) (drop ltNum ln)
                    else [])
           in unlines $ (take lnNum txs)++[newLn]
        else
           _tx st 
      (typ,str) = eval (_fn st) (_em st) newTx
      rst = if (length$words str)<3
               then str else head$words str
  in (foldl (\acc x -> addTxt x acc) st rst) 
        {_md=0, _fn=if (length (words typ) > 0)
                       then if (head$words typ)=="func"
                              then _fn (addFunc (head$words str,unwords$tail$words str) st)
                              else _fn st
                       else _fn st
        }

curToTxt :: State -> (Lineinfo,Position)
curToTxt st =
  let txs = lines (_tx st) 
      lns = lineStartX (_by st) txs 
      lnInfo = getLn (_x st+_sx st) lns
      lnNum = fst lnInfo
  in  if lnNum>(-1)
        then
          let ln = txs !! lnNum
              txPos = getPosition (_by st) (_x st+_sx st) (_y st) lnInfo ln 
           in (lnInfo,txPos) 
        else (lnInfo,(0,0)) 

getLn :: LineNum -> [LineNum] -> Lineinfo 
getLn = getLn' 0 where 
  getLn' _ _ (y:[]) = ((-1),(y,y+1))
  getLn' i x (y:z:ys) =
     if (x>=y && x<z) then (i,(y,z))
                      else getLn' (i+1) x (z:ys)

getPosition :: BtY -> CurX -> CurY -> Lineinfo -> String -> (PosInLine,SpAftLine)
getPosition by x y lni ls =
  let lastLetters = (length ls) - (by+1)*(x-x0) 
   in if (x==x1-1) && lastLetters<y
          then (length ls,y-lastLetters)
          else ((by+1)*(x-x0)+y,0)
  where x0 = fst$snd lni
        x1 = snd$snd lni

numAndOsd :: [(Char,Char)]
numAndOsd = [('1','ひ'),('2','ふ'),('3','み'),('4','よ'),('5','ゐ'),('6','む'),
             ('7','な'),('8','や'),('9','こ'),('0','ろ'),('-','き'),('+','と'),('*','を'),('%','す')]

osdToInt :: String -> Int
osdToInt wd = read $
  map (\c -> foldl (\acc (n,o) -> if (c `elem` "0123456789")
                                     then c
                                     else if (o==c) then n else acc) ' ' numAndOsd ) wd 
  
numToOsd :: Bool -> String -> String
numToOsd evm wd =
  map (\c -> foldl (\acc (n,o) -> if(evm==True && c `elem` "0123456789")
                                     then c
                                     else if (n==c) then o else acc) ' ' numAndOsd ) wd 

osdToNum :: String -> (Int,Int)
osdToNum wd =
  let (_,numTxt) = foldl (\(fl,acc) x ->
        if x=='を' then (True,acc++" ")
                   else if x=='す' then (False,acc++" ")
                                   else if fl then (True,acc++[x]) else (False,acc)) (True,[]) wd
      (_,denTxt) = foldl (\(fl,acc) x ->
        if x=='を' then (False,acc++" ")
                   else if x=='す' then (True,acc++" ")
                                   else if fl then (True,acc++[x]) else (False,acc)) (False,[]) wd
      num = foldl (\acc x -> acc * osdToInt x) 1 (words numTxt)
      den = foldl (\acc x -> acc * osdToInt x) 1 (words denTxt)
   in (num,den) 

showRatio :: Bool -> Ratio Int -> String
showRatio evm rt = let rat = show rt in
  if (last$words rat)=="1" then numToOsd evm$head$words rat
                               else numToOsd evm$concat$words rat

osdToRatio :: String -> Ratio Int 
osdToRatio str = let strs = sepNumOp str in
  foldl (\acc x -> acc+(fst (osdToNum x))%(snd (osdToNum x))) 0 strs

sepNumOp :: String -> [String]
sepNumOp str = foldl (\acc x ->
      if x=='き' || acc==[] then acc++[[x]] else if x=='と'
                then acc++[[]] else (init acc)++[(last acc)++[x]]
                     ) [] str


typeHa :: String -> String
typeHa str
  | foldl (\acc x -> acc && (elem x ((map snd numAndOsd)++"0123456789"))) True str = "ratio"
  | otherwise = "txt"

getHa :: String -> (Int,(Fbody,Fname))
getHa str =
  let wds = if (str=="") then [] 
                         else words $ last $ lines str
      lng = foldr (\x acc -> case () of
              _ |x=='は' && (snd acc)==' ' -> (fst acc+1,'x')
                |snd acc=='x'              -> if (x==' ') then(fst acc+1,'y')
                                                          else (fst acc+1,'x')
                |snd acc=='y'              -> (fst acc,'y')
              otherwise                    -> (fst acc+1,x)
                  ) (0,'s') str
      res = foldr (\x acc -> case () of
              _ |fst acc                      -> if last x=='は' 
                                            then (False,(fst$snd acc,init x)) 
                                            else (True,(x:(fst$snd acc),[]))
              otherwise                       -> (False,(fst$snd acc,snd$snd acc))
                  ) (True,([],[])) wds 
   in if ((fst$snd res)==[]) then (0,([],[])) else (fst lng,(unwords$fst$snd res,snd$snd res))

addFunc :: (Fname,Fbody) -> State -> State
addFunc (nm,bd) st =
  let nmList = map fst (_fn st)
      eln = getIndex nm nmList
  in if eln > (-1) 
        then st {_fn = (take eln (_fn st))++[(nm,bd)]++(drop eln (_fn st))}
        else st {_fn = (_fn st) ++ [(nm,bd)]}

preFunc :: (Fname,Fbody) -> (Fname,[([String],[String])])
preFunc (nm,bd) =
  let bds = words bd
      spni = separate "に" bds
      arg = if (elem "か" bds) then map (head.separate "か") spni 
                                else [[]]
      exp = map (last.separate "か") spni 
   in (nm,(zip arg exp))

separate :: String -> [String] -> [[String]]
separate w wds = let eln = getIndex w wds
                     fs = if eln>0 then take eln wds else []
                     sn = if eln>0 then drop (eln+1) wds else wds
                  in if fs==[] then [wds]
                               else fs:(separate w sn)

pMatch :: [[String]] -> [[String]] -> [String] -> [String]
pMatch = pMatch' 0 
  where 
    pMatch' 0 [[]] fb _ = head fb 
    pMatch' _ [] _ _ = []
    pMatch' i (x:xs) fb arg 
      | foldl (\acc (a,b) -> if (typeHa a=="ratio" && typeHa b=="ratio" && not (a==b))
                                then False else acc) True (zip x arg)
        = foldl (\acc y -> acc 
            ++ [snd$(foldl (\ac z ->
                    (fst ac+1,concat$map (\p -> if p=="x" then (arg!!(fst ac)) else p) (words$sepWith z (snd ac)))) (0,y) x)] 
                ) [] (fb!!i)
      | otherwise = pMatch' (i+1) xs fb arg

sepWith :: String -> String -> String
sepWith wd str 
  | length str >= length wd =if (take (length wd) str)==wd
                                then " x "++(sepWith wd (drop (length wd) str)) 
                                else [(head str)]++(sepWith wd (tail str))
  | otherwise = str
                           
arrStr :: String -> String
arrStr str = case (typeHa str) of
               "ratio" -> showRatio False$osdToRatio str
               _       -> str

ebody :: [(Fname,Fbody)] -> String -> String -> Ftype ->  [String] -> [String] -> (Ftype,[String])
ebody _ pe na ft acc [] = (ft,acc)
ebody fn pe na ft acc (x:xs)
  | getIndex x nmList>(-1) && na==x = ebody fn pe na "txt" (acc++[x]) xs
  | getIndex x nmList>(-1)          = let (nm,bd) = preFunc (fn!!(getIndex x nmList))
                                          accInit = take (length acc - (length$fst$head bd)) acc
                                          accArg  = drop (length acc - (length$fst$head bd)) acc
                                          accArg' = map arrStr accArg
                                          rpl     = pMatch (map fst bd) (map snd bd) accArg'
                                      in ebody fn pe na ft accInit (rpl++xs)
  | not (acc==[]) && (typeHa x=="ratio") && ft=="ratio"
        && (((last$last acc)=='を' || (last$last acc)=='す') || ((head x=='を') || (head x=='す')))
                                 = ebody fn pe na ft ((init acc)++[(last acc)++x]) xs
  | acc==[]                      = ebody fn pe na (typeHa x) (acc++[x]) xs
  | typeHa x=="txt"              = ebody fn pe na "txt" (acc++[x]) xs
  | otherwise                    = ebody fn pe na ft (acc++[x]) xs
  where nmList = map fst fn

eval :: [(Fname,Fbody)] -> Bool -> String -> (Ftype, String)
eval fn evm str =
  let tgt = fst$snd$getHa str
      name = snd$snd$getHa str
   in if (tgt==[])
        then ([],"なし")
        else
          let wds = words tgt 
              peval = str
              ebd = ebody fn peval name "" [] wds
              result = case (fst ebd) of
                         "ratio" -> showRatio evm$foldl (\acc x -> acc+(osdToRatio x)) 0 (snd ebd)
                         _       -> foldl (\acc x -> acc++" "++x) "" (snd ebd)
          in case (fst ebd) of
               "ratio" -> if (name==[]) then ("ratio",result)
                                        else ("func ratio",name++" "++result)
               _       -> if (name==[]) then (fst ebd,result)
                                        else ("func "++(fst ebd),name++" "++result)


loadPictures :: IO (Picture,[Maybe Picture])
loadPictures = do
  Just hime <- loadJuicy "img/hime.png"
  osds <- mapM (\x -> do y <- x;return y) (loadOsds 69)
  return (hime,osds) 

loadOsds :: Int -> [IO (Maybe Picture)]
loadOsds 0 = [loadJuicy "img/osd/osd0.png"]
loadOsds i = loadOsds (i-1)++[(loadJuicy ("img/osd/osd"++(show i)++".png"))]

main :: IO ()
main = do
  (hime',osds') <- loadPictures
  let st' = initState {hime = hime', osds = osds'}
  play window black 10 st' drawPic updateState nextPic
