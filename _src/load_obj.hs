module Main where

import qualified Data.Map as Map
import Data.List.Split (splitOn)

data ObjData = ObjVertex |
               ObjTextureCoords |
               ObjNormal


procObjLine [] st = st

procObjLine ["#" : _] st = st

procObjLine ["usemtl" : _] st = st

procObjLine ["mtllib" : _] st = st

procObjLine ["o" : _] st = st

procObjLine ["s" : _] st = st

procObjLine ["g" : _] st = st

procObjLine ["vp" : _] st = st


procObjLine ["v" : vdata] ((vs,vt,vn),faces) =
    let val = map read vdata :: [Double]
        vs2 = val : vs
    in ((vs2, vt, vn), faces)

procObjLine ["vt" : vdata] ((vs,vt,vn),faces) =
    let val = map read vdata :: [Double]
        vt2 = val : vt
    in ((vs, vt2, vn), faces)

procObjLine ["vn" : vdata] ((vs,vt,vn),faces) =
    let val = map read vdata :: [Double]
        vn2 = val : vn
    in ((vs, vt, vn2), faces)

procObjLine ["vn" : vdata] ((vs,vt,vn),faces) =
    let val = map read vdata :: [Double]
        vn2 = val : vn
    in ((vs, vt, vn2), faces)


procIndex [vi, ti, ni] =


procIndex [vi, "", ni] =


procIndex [vi, ti] =



procObjLine ["f", v1, v2, v3] (vertices, faces) = 
    let d1 = splitOn "/" v1










main :: IO ()
main = do
    inputData <- readFile "part2.txt"
    let content = map words $ lines inputData
        (vertices, faces) = Map.foldr' procObjLine ([],[]) content

