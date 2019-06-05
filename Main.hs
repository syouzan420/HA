module Main where

import Graphics.Gloss 
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.IO.Game
import Data.Ratio

type Lineinfo = (Int,(Float,Float))
type Position = (Int,Int)
type Fname = String
type Fbody = String
type Ftype = String

startX, startY, bottomY, leftX, shiftX, shiftY, fScale :: Float 
startX = 250 
startY = 250 
bottomY = 19 
leftX = 17 
shiftX = 30
shiftY = 24
fScale = 1 

data State = State
  { _x :: Float             -- cursor X
  , _y :: Float             -- cursor Y
  , _by :: Float            -- bottom Y 
  , _lx :: Float            -- left X
  , _sx :: Float            -- scroll X
  , _md :: Int              -- input mode (0:normal 1:insert 2:insert2)
  , _tc :: Char             -- temporary stored character
  , _tx :: String           -- text
  , _fn :: [(Fname,Fbody)]  -- functions (name, body)
  , _info :: String 
  , _key :: Key            
  , _ks :: KeyState
  , _mdf :: Modifiers
  , _cfk :: Int             -- count for key
  , hime :: Picture
  , osds :: [Maybe Picture]
  }

osdAndNum :: [(Char,Int)]
osdAndNum = [('あ',0),('い',1),('う',2),('え',3),('お',4),('か',15),('き',19),('く',23),('け',27),('こ',31)
            ,('さ',55),('し',51),('す',46),('せ',42),('そ',37),('た',53),('ち',49),('つ',44),('て',40),('と',35)
            ,('な',17),('に',21),('ぬ',25),('ね',29),('の',33),('は',16),('ひ',20),('ふ',24),('へ',28),('ほ',32)
            ,('ま',18),('み',22),('む',26),('め',30),('も',34),('や',56),('ゐ',52),('ゆ',47),('ゑ',43),('よ',38)
            ,('ら',54),('り',50),('る',45),('れ',41),('ろ',36),('わ',57),('を',39),('ん',48),(' ',14)
            ,('が',(-15)),('ぎ',(-19)),('ぐ',(-23)),('げ',(-27)),('ご',(-31))
            ,('ざ',(-55)),('じ',(-51)),('ず',(-46)),('ぜ',(-42)),('ぞ',(-37))
            ,('だ',(-53)),('ぢ',(-49)),('づ',(-44)),('で',(-40)),('ど',(-35))
            ,('ば',(-16)),('び',(-20)),('ぶ',(-24)),('べ',(-28)),('ぼ',(-32))]

getSecond :: (Eq a,Num b) => [(a,b)] -> a -> b
getSecond [] _ = 999
getSecond (x:xs) y
  | fst x==y  = snd x
  | otherwise = getSecond xs y

converter :: Char -> Int
converter = getSecond osdAndNum

window :: Display
window = InWindow "は" (640,640) (100,0)

testText :: String
testText = "あきのたの かりほのいほの とまをあらみわがころもでは つゆにぬれつつ\nきみがよは ちよにやちよに さざれいしのいはをとなりて こけのむすまで\n\nあかはなま いきひにみうく"

initState :: State
initState = State {_x=0, _y=0, _by=bottomY, _lx=leftX, _sx=0, _md=0 
                  , _tc=' ', _tx=testText, _fn=[], _info = "" 
                  , _key = Char ' ', _ks = Up, _mdf = Modifiers{shift=Up,ctrl=Up,alt=Up}
                  ,_cfk = 0, hime=blank, osds=[Just blank]}

drawPic :: State -> Picture
drawPic st = 
  let himeB = translate (startX+40) (startY+30) $ scale 2 2 $ hime st
      numList = map (map converter) $ lines $ _tx st
      line = lineStartX (_by st) numList
      picOsds = makePicOsds numList line st
      picDaku = makePicDaku numList line st
      status = translate (startX-shiftX*leftX) (startY-shiftY*(bottomY+3)) $
                  color green $ scale 0.3 0.3 $ text (_info st)
      cursor = translate (startX-shiftX*_x st) (startY-shiftY*_y st+shiftY/2) $
                  color orange $ rectangleSolid 28 2
   in pictures $ cursor:status:himeB:picOsds++picDaku

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
                      txos [(a,b)|a<-[(ln-_sx st)..],b<-[0..(_by st)]]) txOsds line

makePicDaku :: [[Int]] -> [Float] -> State -> [Picture]
makePicDaku numList line st =
  let Just daku = osds st !! 58
   in concat $ zipWith (\nml ln -> zipWith (\t (x,y) -> 
        if(t<0) then translate (startX-shiftX*x) (startY-shiftY*y) $ scale fScale fScale $ daku
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
      st' = st{_cfk=newCfk, _info="mode: "++mode++"  key: "++keyn}
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
          let blankLines = take (round (_x st + _sx st - (last lns))) (repeat [])
              sp = take (round (_y st)) (repeat ' ')
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
                    then snd $ foldl (\acc x -> if (fst acc && x=='た')
                           then (False,snd acc++[x]) else if fst acc
                              then (True,snd acc++[x]) else (False,snd acc)
                               ) (True,[]) (drop ltNum ln)
                    else [])
           in unlines $ (take lnNum txs)++[newLn]
        else
           _tx st 
      (typ,str) = eval (_fn st) newTx
      rst = if (length$words str)<3
               then str else head$words str
  in (foldl (\acc x -> addTxt x acc) st rst) 
        {_md=0, _fn=if (head$words typ)=="func"
                       then _fn (addFunc (head$words str,unwords$tail$words str) st)
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
              txPos = getPosition (_by st) (_x st+_sx st) (_y st)
                  (fst$snd lnInfo) (snd$snd lnInfo) ln 
           in (lnInfo,txPos) 
        else (lnInfo,(0,0)) 


getLn :: Float -> [Float] -> Lineinfo 
getLn = getLn' 0 where 
  getLn' _ _ (y:[]) = ((-1),(y,y+1))
  getLn' i x (y:z:ys) =
     if (x>=y && x<z) then (i,(y,z))
                      else getLn' (i+1) x (z:ys)

getPosition :: Float -> Float -> Float -> Float -> Float -> [a] -> Position 
getPosition by x y x0 x1 ls =
  let lastLetters = fromIntegral (length ls) - (by+1)*(x-x0) 
   in if (x==(x1-1)) && lastLetters<y
          then (length ls,round (y-lastLetters))
          else (round ((by+1)*(x-x0)+y),0)


numAndOsd :: [(Char,Char)]
numAndOsd = [('1','ひ'),('2','ふ'),('3','み'),('4','よ'),('5','ゐ'),('6','む'),
             ('7','な'),('8','や'),('9','こ'),('0','ろ'),('-','き'),('+','と'),('*','を'),('%','す')]

osdToInt :: String -> Int
osdToInt wd =
  read $ map (\c -> foldl (\acc s ->
                if (snd s==c) then (fst s) else acc) ' ' numAndOsd 
             ) wd 

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

numToOsd :: String -> String
numToOsd str =
  map (\c -> foldl (\acc s ->
              if (fst s==c) then (snd s) else acc) ' ' numAndOsd 
      ) str 

showRatio :: Ratio Int -> String
showRatio rt =
  let rat = show rt
   in if (last$words rat)=="1" then numToOsd$head$words rat
                               else numToOsd$concat$words rat

typeHa :: String -> String
typeHa str
  | foldl (\acc x -> acc && (elem x (map snd numAndOsd))) True str = "ratio"
  | otherwise = "txt"

getHa :: String -> (Int,(Fbody,Fname))
getHa str =
  let wds = words str
      lng = foldr (\x acc ->
              if (x=='は' && (snd acc)==' ')
                 then (fst acc+1,'x')
                 else if (snd acc=='x')
                        then if (x==' ')
                                then (fst acc+1,'y')
                                else (fst acc+1,'x')
                        else if(snd acc=='y')
                                then (fst acc,'y')
                                else (fst acc+1,x)
                  ) (0,'s') str
      res = foldr (\x acc ->
               if (fst$snd acc)==[] && x=="た"
                 then (True,([],[])) else if fst acc
                        then if last x=='は' 
                                then (False,(fst$snd acc,init x)) 
                                else (True,(x:(fst$snd acc),[]))
                        else (False,(fst$snd acc,snd$snd acc))
                                      ) (False,([],[])) wds 
   in if ((fst$snd res)==[]) then (0,([],[])) else (fst lng,(unwords$fst$snd res,snd$snd res))

osdToRatio :: String -> Ratio Int 
osdToRatio str = 
  let strs = sepNumOp str
   in foldl (\acc x -> acc+(fst (osdToNum x))%(snd (osdToNum x))) 0 strs

sepNumOp :: String -> [String]
sepNumOp str = foldl (\acc x ->
      if x=='き' || acc==[] 
          then acc++[[x]] else if x=='と'
                then acc++[[]] else (init acc)++[(last acc)++[x]]
                     ) [] str

eval :: [(Fname,Fbody)] -> String -> (Ftype, String)
eval fn str =
  let tgt = fst$snd$getHa str
      name = snd$snd$getHa str
   in if (tgt==[])
        then ([],"なし")
        else
          let wds = words tgt 
              peval = take (length str - (fst$getHa str)) str
              ebd = ebody fn peval "" [] wds
              result = case (fst ebd) of
                         "ratio" -> showRatio$foldl (\acc x -> acc+(osdToRatio x)) 0 (snd ebd)
                         _       -> foldl (\acc x -> acc++" "++x) "" (snd ebd)
          in case (fst ebd) of
               "ratio" -> if (name==[]) then ("ratio",result)
                                        else ("func ratio",name++" "++result)
               _       -> if (name==[]) then (fst ebd,result)
                                        else ("func "++(fst ebd),name++" "++result)

ebody :: [(Fname,Fbody)] -> String -> Ftype ->  [String] -> [String] -> (Ftype,[String])
ebody _ pe ft acc [] = (ft,acc)
ebody fn pe ft acc (x:xs)
  | elNum x nmList > (-1) = let (nm,bd) = preFunc (fn!!(elNum x nmList))
                                accInit = take (length acc - (length$fst$head bd)) acc
                                accArg = drop (length acc - (length$fst$head bd)) acc
                                rpl = pMatch (map fst bd) (map snd bd) accArg
                             in ebody fn pe ft accInit (rpl++xs)
  | length x > 1 = let cap = take 2 x
                    in if cap=="まへ"
                          then ebody fn pe ft acc ((words$fst$snd$getHa pe)++xs)
                          else ebody fn pe (typeHa x) (acc++[arrStr (typeHa x) x]) xs
  | acc==[] = ebody fn pe (typeHa x) (acc++[arrStr (typeHa x) x]) xs
  | ((head x=='を') || (head x=='す')) && ft=="ratio"
          = if (typeHa (tail x)=="ratio") 
                then ebody fn pe ft ((init acc)++[arrStr ft ((last acc)++x)]) xs
                else ebody fn pe (typeHa x) (acc++[arrStr (typeHa x) x]) xs
  | (typeHa x=="ratio") && ft=="ratio" && ((last$last acc)=='を' || (last$last acc)=='す')
          = ebody fn pe ft ((init acc)++[arrStr ft ((last acc)++x)]) xs
  | otherwise = ebody fn pe ft (acc++[arrStr ft x]) xs
  where nmList = map fst fn

arrStr :: Ftype -> String -> String
arrStr ft str = case ft of
               "ratio" -> case (typeHa str) of
                            "ratio" -> showRatio$osdToRatio str
                            _       -> str
               _       -> str

elNum :: String -> [String] -> Int
elNum = elNum' 0 where
  elNum' _ _ [] = (-1) 
  elNum' i str (x:xs)
    | str==x = i
    | otherwise = elNum' (i+1) str xs 

addFunc :: (Fname,Fbody) -> State -> State
addFunc (nm,bd) st =
  let nmList = map fst (_fn st)
      eln = elNum nm nmList
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
separate w wds = let eln = elNum w wds
                     fs = if eln>0 then take eln wds else []
                     sn = if eln>0 then drop (eln+1) wds else wds
                  in if fs==[] then [wds]
                               else fs:(separate w sn)


  {--
arrFunc :: [(Fname,Fbody)] -> [String] -> [String]
arrFunc fn wds = foldl (\acc x -> 
  if elem x nmList
     then let (nm,bd) = preFunc (fn!!(elNum x nmList)) 
              accInit = take (length acc - (length$fst$head bd)) acc
              accArg = drop (length acc - (length$fst$head bd)) acc
           in accInit ++ rplArg (fst$head bd) (snd$head$bd) accArg
     else acc ++ [x]
          ) [] wds
      where nmList = map fst fn 
--}
--

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
                    (fst ac+1,concat$map (\p -> if p=="x" then (arg!!(fst ac)) else p) (words$sepWith z y))) (0,[]) x)] 
                ) [] (fb!!i)
      | otherwise = pMatch' (i+1) xs fb arg

sepWith :: String -> String -> String
sepWith wd str 
  | length str >= length wd =if (take (length wd) str)==wd
                                then " x "++(sepWith wd (drop (length wd) str)) 
                                else [(head str)]++(sepWith wd (tail str))
  | otherwise = str

rplArg :: [String] -> [String] -> [String] -> [String]
rplArg anm fnb arg = foldl (\acc x ->
  if elem x anm then acc ++ [arg!!(elNum x anm)]
                else acc ++ [x]
                           ) [] fnb
  

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
  play window black 10 st' drawPic updateState nextPic
