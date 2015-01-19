{-
This is module provides functions to read in the Emitters.yml configuration file
and convert to the format used by Emitters/ServiceConfiguration.hs
-}


module Gannet.Emitters.ConfigReader (
    readRuntimeConfig
) where

import Data.Yaml.Syck
import Data.Char (toLower)
import System.IO.Unsafe
                                                                   
readRuntimeConfig cfgfile =
    let
        yml = unsafePerformIO $ parseYamlFile cfgfile
        (cm,am) = getRuntimeYMaps yml
    in
        ((classYMapToList cm),(aliasYMapToList am))

getRuntimeYMaps yml = 
    let 
            EMap ltry = (n_elem yml)
            (_,hv)   = head ltry
            EMap [classYMap, aliasYMap] = (n_elem hv)
            (_,classYMap_v)=classYMap
            (_,aliasYMap_v)=aliasYMap
    in        
            (classYMap_v,aliasYMap_v)

ymlMapToList helper m =            
    let
        ym = getYMap m
    in
       map (\mv->helper mv ) ym                                    
    
classYMapToList = ymlMapToList classYMapToList_helper
classYMapToList_helper (mk,mv)  = (map toLower (getYStr mk), getYStr mv)
   
aliasYMapToList = ymlMapToList aliasYMapToList_helper
aliasYMapToList_helper (mk,mv)  = (map toLower (getYStr mk), map toLower (getYStr mv))


getYMap m = 
    let
        EMap sm = (n_elem m)
    in
        sm
{-
getYSeq l =
    let
        ESeq sl = (n_elem l)
    in
        sl
-}

getYStr mk = 
    let 
        EStr k = n_elem mk
    in
        unpackBuf k

