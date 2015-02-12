module Translation where

import OpCode
import ByteMath
import Indexing
import Tagging
import Data.Word
import VarBindings
import Abs



type AbstractProg = ((IndexLookup Command), Int, [[Word8]], (TagLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), [[Word8]], (IndexLookup Int), (TagLookup Reference), VarManager, [Word8])

ap_getCode :: AbstractProg -> (IndexLookup Command)           -- get Bytecode Commands
ap_getPos :: AbstractProg -> Int                              -- get current Position
ap_getCPStr :: AbstractProg -> [[Word8]]                      -- get String ConstPool
ap_getCPStrLookup :: AbstractProg -> (TagLookup Int)          -- get String ConstPool Lookup
ap_getCPClass :: AbstractProg -> [[Word8]]                    -- get Class Index ConstPool
ap_getCPClassLookup :: AbstractProg -> (IndexLookup Int)      -- get Class Index ConstPool Lookup
ap_getCPType :: AbstractProg -> [[Word8]]                     -- get Type Index ConstPool
ap_getCPTypeLookup :: AbstractProg -> (IndexLookup Int)       -- get Type Index ConstPool Lookup
ap_getCPField :: AbstractProg -> [[Word8]]                    -- get Field Index ConstPool
ap_getCPFieldLookup :: AbstractProg -> (IndexLookup Int)      -- get Field Index ConstPool Lookup
ap_getCPMethod :: AbstractProg -> [[Word8]]                   -- get Method Index ConstPool
ap_getCPMethodLookup :: AbstractProg -> (IndexLookup Int)     -- get Method Index ConstPool Lookup
ap_getCPIFMethod :: AbstractProg -> [[Word8]]                 -- get Interface Method Index ConstPool
ap_getCPIFMethodLookup :: AbstractProg -> (IndexLookup Int)   -- get Interface Method Index ConstPool Lookup
ap_getCPIndex :: AbstractProg -> [[Word8]]                    -- get String Index ConstPool
ap_getCPIndexLookup :: AbstractProg -> (IndexLookup Int)      -- get String Index ConstPool Lookup
ap_getCPInt :: AbstractProg -> [[Word8]]                      -- get Integer ConstPool
ap_getCPIntLookup :: AbstractProg -> (IndexLookup Int)        -- get Integer ConstPool Lookup
ap_getCPFloat :: AbstractProg -> [[Word8]]                    -- get Float ConstPool
ap_getCPFloatLookup :: AbstractProg -> (IndexLookup Int)      -- get Float ConstPool Lookup
ap_getCPLong :: AbstractProg -> [[Word8]]                     -- get Long ConstPool
ap_getCPLongLookup :: AbstractProg -> (IndexLookup Int)       -- get Long ConstPool Lookup
ap_getCPDouble :: AbstractProg -> [[Word8]]                   -- get Double ConstPool
ap_getCPDoubleLookup :: AbstractProg -> (IndexLookup Int)     -- get Double ConstPool Lookup
ap_getNamePool :: AbstractProg -> (TagLookup Reference)       -- get Name Pool
ap_getVarManager :: AbstractProg -> VarManager                -- get Variable Binding Manager
ap_getAccFlags :: AbstractProg -> [Word8]                     -- get Access Flags

ap_getCode (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cmd
ap_getPos (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = pos
ap_getCPStr (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpS
ap_getCPStrLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luS
ap_getCPClass (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpC
ap_getCPClassLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luC
ap_getCPType (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpT
ap_getCPTypeLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luT
ap_getCPField (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpFd
ap_getCPFieldLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luFd
ap_getCPMethod (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpM
ap_getCPMethodLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luM
ap_getCPIFMethod (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpIM
ap_getCPIFMethodLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luIM
ap_getCPIndex (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpSI
ap_getCPIndexLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luSI
ap_getCPInt (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpI
ap_getCPIntLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luI
ap_getCPFloat (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpF
ap_getCPFloatLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luF
ap_getCPLong (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpL
ap_getCPLongLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luL
ap_getCPDouble (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = cpD
ap_getCPDoubleLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = luD
ap_getNamePool (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = np
ap_getVarManager (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = vm
ap_getAccFlags (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) = accF

ap_setCode :: AbstractProg -> (IndexLookup Command) -> AbstractProg           -- set Bytecode Commands
ap_setPos :: AbstractProg -> Int -> AbstractProg                              -- set current Position
ap_setCPStr :: AbstractProg -> [[Word8]] -> AbstractProg                      -- set String ConstPool
ap_setCPStrLookup :: AbstractProg -> (TagLookup Int) -> AbstractProg          -- set String ConstPool Lookup
ap_setCPClass :: AbstractProg -> [[Word8]] -> AbstractProg                    -- set Class Index ConstPool
ap_setCPClassLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg      -- set Class Index ConstPool Lookup
ap_setCPType :: AbstractProg -> [[Word8]] -> AbstractProg                     -- set Type Index ConstPool
ap_setCPTypeLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg       -- set Type Index ConstPool Lookup
ap_setCPField :: AbstractProg -> [[Word8]] -> AbstractProg                    -- set Field Index ConstPool
ap_setCPFieldLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg      -- set Field Index ConstPool Lookup
ap_setCPMethod :: AbstractProg -> [[Word8]] -> AbstractProg                   -- set Method Index ConstPool
ap_setCPMethodLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg     -- set Method Index ConstPool Lookup
ap_setCPIFMethod :: AbstractProg -> [[Word8]] -> AbstractProg                 -- set Interface Method Index ConstPool
ap_setCPIFMethodLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg   -- set Interface Method Index ConstPool Lookup
ap_setCPIndex :: AbstractProg -> [[Word8]] -> AbstractProg                    -- set String Index ConstPool
ap_setCPIndexLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg      -- set String Index ConstPool Lookup
ap_setCPInt :: AbstractProg -> [[Word8]] -> AbstractProg                      -- set Integer ConstPool
ap_setCPIntLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg        -- set Integer ConstPool Lookup
ap_setCPFloat :: AbstractProg -> [[Word8]] -> AbstractProg                    -- set Float ConstPool
ap_setCPFloatLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg      -- set Float ConstPool Lookup
ap_setCPLong :: AbstractProg -> [[Word8]] -> AbstractProg                     -- set Long ConstPool
ap_setCPLongLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg       -- set Long ConstPool Lookup
ap_setCPDouble :: AbstractProg -> [[Word8]] -> AbstractProg                   -- set Double ConstPool
ap_setCPDoubleLookup :: AbstractProg -> (IndexLookup Int) -> AbstractProg     -- set Double ConstPool Lookup
ap_setNamePool :: AbstractProg -> (TagLookup Reference) -> AbstractProg       -- set Name Pool
ap_setVarManager :: AbstractProg -> VarManager -> AbstractProg                -- set Variable Binding Manager
ap_setAccFlags :: AbstractProg -> [Word8] -> AbstractProg                     -- set Access Flags

ap_setCode (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (x, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setPos (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, x, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPStr (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, x, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPStrLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, x, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPClass (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, x, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPClassLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, x, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPType (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, x, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPTypeLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, x, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPField (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, x, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPFieldLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, x, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPMethod (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, x, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPMethodLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, x, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPIFMethod (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, x, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPIFMethodLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, x, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPIndex (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, x, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPIndexLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, x, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPInt (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, x, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPIntLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, x, cpF, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPFloat (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, x, luF, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPFloatLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, x, cpL, luL, cpD, luD, np, vm, accF)
ap_setCPLong (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, x, luL, cpD, luD, np, vm, accF)
ap_setCPLongLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, x, cpD, luD, np, vm, accF)
ap_setCPDouble (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, x, luD, np, vm, accF)
ap_setCPDoubleLookup (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, x, np, vm, accF)
ap_setNamePool (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, x, vm, accF)
ap_setVarManager (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, x, accF)
ap_setAccFlags (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, accF) x = (cmd, pos, cpS, luS, cpC, luC, cpT, luT, cpFd, luFd, cpM, luM, cpIM, luIM, cpSI, luSI, cpI, luI, cpF, luF, cpL, luL, cpD, luD, np, vm, x)

ap_new = (NewIndexLookup, (0 :: Int), [], NewTagLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, [], NewIndexLookup, NewTagLookup, NewVarManager, [0, 0])   -- new (empty) AbstactProg

ap_incPos :: AbstractProg -> AbstractProg          -- increment current Position
ap_incPos ap = ap_setPos ap ((ap_getPos ap) + 1)

ap_addCmd ::  AbstractProg -> Command -> AbstractProg                               -- add new Codeline
ap_addCmd ap c = ap_incPos (ap_setCode ap ((ap_getCode ap) !# (ap_getPos ap) # c))


data Reference = RefToV_I Int | RefToC_I Int | RefToV_F Int | RefToC_F Int | RefToV_L Int | RefToC_L Int | RefToV_D Int | RefToC_D Int | RefToV_A Int | RefToC_A Int | RefToC_Class Int | UndefConst | RefVal_m1 | RefVal_0 | RefVal_1 | RefVal_2 | RefVal_3 | RefVal_4 | RefVal_5 | StackRef | CodeRef Int | NullRef | UnusedName deriving Show



-- Push Value (at referenced Index) to Stack of AP

pushToStack :: AbstractProg -> Reference -> AbstractProg
pushToStack ap (RefToV_I 0) = ap_addCmd ap Cmd_ILoad0
pushToStack ap (RefToV_I 1) = ap_addCmd ap Cmd_ILoad1
pushToStack ap (RefToV_I 2) = ap_addCmd ap Cmd_ILoad2
pushToStack ap (RefToV_I 3) = ap_addCmd ap Cmd_ILoad3
pushToStack ap (RefToV_I i) | (i < 256) = ap_addCmd ap (Cmd_ILoad (useValueOf i))
                            | otherwise = ap_addCmd ap (Cmd_Wide_ILoad (head (fromShort (useValueOf i))) (head (tail (fromShort (useValueOf i)))))
pushToStack ap (RefToC_I i) | (i < 256) = ap_addCmd ap (Cmd_LdC_I (useValueOf i)) 
                            | otherwise = ap_addCmd ap (Cmd_LdCW_I (head (fromShort (useValueOf i))) (head (tail (fromShort (useValueOf i)))))

-- Float, Long , Double Refs not implemented

pushToStack ap (RefToV_A 0) = ap_addCmd ap Cmd_ALoad0
pushToStack ap (RefToV_A 1) = ap_addCmd ap Cmd_ALoad1
pushToStack ap (RefToV_A 2) = ap_addCmd ap Cmd_ALoad2
pushToStack ap (RefToV_A 3) = ap_addCmd ap Cmd_ALoad3
pushToStack ap (RefToV_A i) | (i < 256) = ap_addCmd ap (Cmd_ALoad (useValueOf i))
                            | otherwise = ap_addCmd ap (Cmd_Wide_ALoad (head (fromShort (useValueOf i))) (head (tail (fromShort (useValueOf i)))))
pushToStack ap (RefToC_A i) | (i < 256) = ap_addCmd ap (Cmd_LdC_S (useValueOf i)) 
                            | otherwise = ap_addCmd ap (Cmd_LdCW_S (head (fromShort (useValueOf i))) (head (tail (fromShort (useValueOf i)))))

pushToStack ap UndefConst = ap_addCmd ap Cmd_IConst0   -- should not occur in correct inputs (just in case)
pushToStack ap RefVal_m1 = ap_addCmd ap Cmd_IConstM1
pushToStack ap RefVal_0 = ap_addCmd ap Cmd_IConst0
pushToStack ap RefVal_1 = ap_addCmd ap Cmd_IConst1
pushToStack ap RefVal_2 = ap_addCmd ap Cmd_IConst2
pushToStack ap RefVal_3 = ap_addCmd ap Cmd_IConst3
pushToStack ap RefVal_4 = ap_addCmd ap Cmd_IConst4
pushToStack ap RefVal_5 = ap_addCmd ap Cmd_IConst5
pushToStack ap StackRef = ap                           -- nothing to be pushed, result is already on Stack
pushToStack ap NullRef = ap_addCmd ap Cmd_AConstNull
pushToStack ap UnusedName = ap                         -- never to be called



popToVar :: AbstractProg -> String -> String -> AbstractProg
popToVar ap name typename = let unpackRef Nothing = UnusedName
                                unpackRef (Just x) = x
                            in let isUnused UnusedName = True
                                   isUnused x = False
                               in let ref = unpackRef ((ap_getNamePool ap) ?% name)
                                  in if (isUnused ref) then (insertAsNewVar ap name typename) else (writeToRef ap name ref)

writeToRef :: AbstractProg -> String -> Reference -> AbstractProg
writeToRef ap name (RefToV_I 0) = ap_addCmd ap Cmd_IStore0
writeToRef ap name (RefToV_I 1) = ap_addCmd ap Cmd_IStore1
writeToRef ap name (RefToV_I 2) = ap_addCmd ap Cmd_IStore2
writeToRef ap name (RefToV_I 3) = ap_addCmd ap Cmd_IStore3
writeToRef ap name (RefToV_I ref) | (ref < 256) = ap_addCmd ap (Cmd_IStore (useValueOf ref))
                                  | otherwise = ap_addCmd ap (Cmd_Wide_IStore (head (fromShort (useValueOf ref))) (head (tail (fromShort (useValueOf ref)))))
-- Float, Long , Double Refs not implemented
writeToRef ap name (RefToV_A 0) = ap_addCmd ap Cmd_AStore0
writeToRef ap name (RefToV_A 1) = ap_addCmd ap Cmd_AStore1
writeToRef ap name (RefToV_A 2) = ap_addCmd ap Cmd_AStore2
writeToRef ap name (RefToV_A 3) = ap_addCmd ap Cmd_AStore3
writeToRef ap name (RefToV_A ref) | (ref < 256) = ap_addCmd ap (Cmd_AStore (useValueOf ref))
                                  | otherwise = ap_addCmd ap (Cmd_Wide_AStore (head (fromShort (useValueOf ref))) (head (tail (fromShort (useValueOf ref)))))
writeToRef ap name NullRef = insertAsNewVar ap name ""
writeToRef ap name x = ap

insertAsNewVar :: AbstractProg -> String -> String -> AbstractProg
insertAsNewVar ap name "int" = let newAP = ap_setVarManager ap (snd (bindV (ap_getVarManager ap)))
                               in let newRef = fst (bindV (ap_getVarManager ap))
                                  in let newAP2 = ap_setNamePool newAP ((ap_getNamePool newAP) !% name % (RefToV_I newRef))
                                     in writeToRef newAP2 name (RefToV_I newRef)
insertAsNewVar ap name reftype = let newAP = ap_setVarManager ap (snd (bindV (ap_getVarManager ap)))
                                 in let newRef = fst (bindV (ap_getVarManager ap))
                                    in let newAP2 = ap_setNamePool newAP ((ap_getNamePool newAP) !% name % (RefToV_A newRef))
                                       in writeToRef newAP2 name (RefToV_A newRef)

cleanRef :: (AbstractProg, Reference) -> AbstractProg
cleanRef (ap, StackRef) = (ap_addCmd ap Cmd_Pop)
cleanRef (ap, r) = ap

replaceSurrogates_While :: (IndexLookup Command) -> Int -> Int -> Command -> Command -> (IndexLookup Command)
replaceSurrogates_While il from to cmd_c cmd_b   | (from == to) = il
                                                 | otherwise = let tryReplace (Just Srgt_Continue) = cmd_c
                                                                   tryReplace (Just Srgt_BreakLoop) = cmd_b
                                                                   tryReplace (Just x) = x
                                                                   tryReplace Nothing = Cmd_Nop
                                                               in replaceSurrogates_While (il !# from # (tryReplace (il ?# from))) (from + 1) to cmd_c cmd_b

replaceSurrogates :: AbstractProg -> AbstractProg
replaceSurrogates ap = ap_setCode ap (replaceSurrogates_sub (ap_getCode ap) 0 (ap_getPos ap) (ap_getNamePool ap))

replaceSurrogates_sub :: (IndexLookup Command) -> Int -> Int -> (TagLookup Reference) -> (IndexLookup Command)
replaceSurrogates_sub il from to np | (from == to) = il
                                    | otherwise = let unpackCodeRef (Just (CodeRef i)) = i
                                                      unpackCodeRef x = 0
                                                  in let tryReplace (Just (Srgt_GotoLabel n)) = Cmd_Goto (head (fromShort (useValueOf (unpackCodeRef (np ?% n))))) (head (tail (fromShort (useValueOf (unpackCodeRef (np ?% n))))))
                                                         tryReplace (Just x) = x
                                                         tryReplace Nothing = Cmd_Nop
                                                     in replaceSurrogates_sub (il !# from # (tryReplace (il ?# from))) (from + 1) to np


loopConversion :: Statement -> Statement     -- convert For-Loop to While
loopConversion (For x (Just y) z b) = let sub x = StatementExpStatement x
                                      in Block (x ++ [(While y (Block ([b] ++ (map sub z))))])
loopConversion (For x Nothing z b) = let sub x = StatementExpStatement x
                                     in Block (x ++ [(While (TypedExp (Integer 1) "int") (Block ([b] ++ (map sub z))))])
loopConversion (LabeledStatement n (For x (Just y) z b)) = let sub x = StatementExpStatement x
                                                           in Block (x ++ [(LabeledStatement n (While y (Block ([b] ++ (map sub z)))))])
loopConversion x = x

modToAF :: [Modifier] -> [AccessFlag]
modToAF (Public : xs) = Acc_Public : (modToAF xs)
modToAF (Final : xs) = Acc_Final : (modToAF xs)
modToAF (Private : xs) = modToAF xs
modToAF (Static : xs) = modToAF xs
modToAF (Protected : xs) = modToAF xs
modToAF (Abstract : xs) = Acc_Abstract : (modToAF xs)


-- translate ClassDef

translateC :: ClassDef -> AbstractProg
translateC (ClassDef n mod funcs fields ctors blocks) = let apC = ap_setCPClassLookup (ap_setCPClass (ap_setCPStr (ap_setCPStrLookup ap_new (NewTagLookup !% n % 0)) [(fromString n)]) ([[(translateConstTag CTg_Class), 0, 0]])) (NewIndexLookup !# 0 # 0)
                                                        in let apMod = (ap_setAccFlags apC (setAccessFlags (modToAF mod)))
                                                           in let apFld = translateFld fields apMod
                                                              in let apCtor = translateCtor ctors apFld
                                                                 in let apFunc = translateFunc funcs apCtor
                                                                    in fst (translateS (Block blocks) apFunc)

-- translate Fields

translateFld :: [MemberField] -> AbstractProg -> AbstractProg
translateFld (f : fs) ap = ap --
translateFld [] ap = ap

-- translate Constructors 

translateCtor :: [Constructor] -> AbstractProg -> AbstractProg
translateCtor (f: fs) ap = ap --
translateCtor [] ap = ap

-- translate Methods 

translateFunc :: [MemberFunction] -> AbstractProg -> AbstractProg
translateFunc (f: fs) ap = ap --
translateFunc [] ap = ap

-- translate TypedExp into AbstractProg and return Reference to evaluation

translate :: Exp -> AbstractProg -> (AbstractProg, Reference)

translate (TypedExp (TypedExp x tt) t) ap = translate (TypedExp x tt) ap

translate (TypedExp (String x) t) ap = let u = (addConstStr x CTg_Utf8 (ap_getCPStr ap) (ap_getCPStrLookup ap))
                                       in let s = (addConst (tuple2FromList (fromShort (useValueOf (fst3 u)))) CTg_String (ap_getCPIndex ap) (ap_getCPIndexLookup ap))
                                          in ((ap_setCPIndexLookup (ap_setCPIndex (ap_setCPStrLookup (ap_setCPStr ap (snd3 u)) (trd3 u)) (snd3 s)) (trd3 s)), (RefToC_I (fst3 s)))

translate (TypedExp (Integer (-1)) t) ap = (ap, RefVal_m1)
translate (TypedExp (Integer 0) t) ap = (ap, RefVal_0)
translate (TypedExp (Integer 1) t) ap = (ap, RefVal_1)
translate (TypedExp (Integer 2) t) ap = (ap, RefVal_2)
translate (TypedExp (Integer 3) t) ap = (ap, RefVal_3)
translate (TypedExp (Integer 4) t) ap = (ap, RefVal_4)
translate (TypedExp (Integer 5) t) ap = (ap, RefVal_5)
translate (TypedExp (Integer x) t) ap = let c = (addConst32 (tuple4FromList (fromInt (useValueOf x))) CTg_Int (ap_getCPInt ap) (ap_getCPIntLookup ap))
                                        in ((ap_setCPIntLookup (ap_setCPInt ap (snd3 c)) (trd3 c)), (RefToC_I (fst3 c)))

-- translate (TypedExp (Float 0.0) t) ap = ap
-- translate (TypedExp (Float 1.0) t) ap = ap
-- translate (TypedExp (Float 2.0) t) ap = ap
-- translate (TypedExp (Float x) t) ap = let c = (addConst32 (tuple4FromList (fromFloat (useValueOf x))) CTg_Float (ap_getCPFloat ap) (ap_getCPFloatLookup ap))
--                                       in (ap_setCPFloatLookup (ap_setCPFloat ap (snd3 c)) (trd3 c))

translate (TypedExp (Char x) t) ap = translate (TypedExp (Integer (useValueOf x)) t) ap

translate (TypedExp (Boolean True) t) ap = (ap, RefVal_1)
translate (TypedExp (Boolean False) t) ap = (ap, RefVal_0)

translate (TypedExp (Null) t) ap = (ap, NullRef)

translate (TypedExp (LocalOrFieldVar n) t) ap = let unpackRef Nothing = UnusedName
                                                    unpackRef (Just x) = x
                                                in (ap, unpackRef ((ap_getNamePool ap) ?% n))

-- Infix for int / char (default case) , also used for non-shortcicuited boolean operators &

translate (TypedExp (Infix "+" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IAdd), StackRef)
translate (TypedExp (Infix "*" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IMul), StackRef)
translate (TypedExp (Infix "-" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_ISub), StackRef)
translate (TypedExp (Infix "/" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IDiv), StackRef)
translate (TypedExp (Infix "%" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IRem), StackRef)
translate (TypedExp (Infix "<<" l r) t) ap = let lhs = translate l ap
                                                 in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                    in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IShL), StackRef)
translate (TypedExp (Infix ">>" l r) t) ap = let lhs = translate l ap
                                                 in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                    in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IShR), StackRef)
translate (TypedExp (Infix ">>>" l r) t) ap = let lhs = translate l ap
                                                  in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                     in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IUShR), StackRef)
translate (TypedExp (Infix "&" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IAnd), StackRef)
translate (TypedExp (Infix "|" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IOr), StackRef)
translate (TypedExp (Infix "^" l r) t) ap = let lhs = translate l ap
                                                in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                   in ((ap_addCmd (pushToStack (fst rhs) (snd rhs)) Cmd_IXor), StackRef)
-- Float, Long, Double not implemented (to be implemented above default case)

translate (TypedExp (Infix "&&" l r) t) ap = let lhs = translate l ap
                                             in let newAP = (pushToStack (fst lhs) (snd lhs))
                                                in let fstPos = (ap_getPos newAP)
                                                   in let newAP2 = (ap_addCmd newAP Srgt_Jump)
                                                      in let rhs = translate r newAP2
                                                         in let newAP3 = (pushToStack (fst rhs) (snd rhs))
                                                            in let sndPos = (ap_getPos newAP3)
                                                               in let jumpCmd = Cmd_IfEq (head (fromShort (useValueOf (sndPos + 3)))) (head (tail (fromShort (useValueOf (sndPos + 3)))))
                                                                  in let newAP4 = ap_addCmd newAP3 jumpCmd
                                                                     in let newAP5 = ap_setCode newAP4 ((ap_getCode newAP4) !# fstPos # jumpCmd)
                                                                        in let newAP6 = ap_addCmd (ap_addCmd (ap_addCmd newAP5 Cmd_IConst1) (Cmd_Goto (head (fromShort (useValueOf (sndPos + 4)))) (head (tail (fromShort (useValueOf (sndPos + 4))))))) Cmd_IConst0
                                                                           in (newAP6, StackRef)
translate (TypedExp (Infix "||" l r) t) ap = let lhs = translate l ap
                                             in let newAP = (pushToStack (fst lhs) (snd lhs))
                                                in let fstPos = (ap_getPos newAP)
                                                   in let newAP2 = (ap_addCmd newAP Srgt_Jump)
                                                      in let rhs = translate r newAP2
                                                         in let newAP3 = (pushToStack (fst rhs) (snd rhs))
                                                            in let sndPos = (ap_getPos newAP3)
                                                               in let jumpCmd = Cmd_IfNe (head (fromShort (useValueOf (sndPos + 3)))) (head (tail (fromShort (useValueOf (sndPos + 3)))))
                                                                  in let newAP4 = ap_addCmd newAP3 jumpCmd
                                                                     in let newAP5 = ap_setCode newAP4 ((ap_getCode newAP4) !# fstPos # jumpCmd)
                                                                        in let newAP6 = ap_addCmd (ap_addCmd (ap_addCmd newAP5 Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf (sndPos + 4)))) (head (tail (fromShort (useValueOf (sndPos + 4))))))) Cmd_IConst1
                                                                           in (newAP6, StackRef)

translate (TypedExp (Infix "==" l r) t) ap = let lhs = translate l ap
                                             in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                in let newAP = ap_addCmd (pushToStack (fst rhs) (snd rhs)) (Cmd_IfICmpEq (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4))))))
                                                   in let newAP2 = ap_addCmd (ap_addCmd newAP Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5))))))
                                                      in ((ap_addCmd newAP2 Cmd_IConst1), StackRef)
translate (TypedExp (Infix "!=" l r) t) ap = let lhs = translate l ap
                                             in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                in let newAP = ap_addCmd (pushToStack (fst rhs) (snd rhs)) (Cmd_IfICmpNe (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4))))))
                                                   in let newAP2 = ap_addCmd (ap_addCmd newAP Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5))))))
                                                      in ((ap_addCmd newAP2 Cmd_IConst1), StackRef)
translate (TypedExp (Infix "<" l r) t) ap = let lhs = translate l ap
                                            in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                               in let newAP = ap_addCmd (pushToStack (fst rhs) (snd rhs)) (Cmd_IfICmpLt (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4))))))
                                                  in let newAP2 = ap_addCmd (ap_addCmd newAP Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5))))))
                                                     in ((ap_addCmd newAP2 Cmd_IConst1), StackRef)
translate (TypedExp (Infix ">=" l r) t) ap = let lhs = translate l ap
                                             in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                in let newAP = ap_addCmd (pushToStack (fst rhs) (snd rhs)) (Cmd_IfICmpGe (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4))))))
                                                   in let newAP2 = ap_addCmd (ap_addCmd newAP Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5))))))
                                                      in ((ap_addCmd newAP2 Cmd_IConst1), StackRef)
translate (TypedExp (Infix ">" l r) t) ap = let lhs = translate l ap
                                            in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                               in let newAP = ap_addCmd (pushToStack (fst rhs) (snd rhs)) (Cmd_IfICmpGt (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4))))))
                                                  in let newAP2 = ap_addCmd (ap_addCmd newAP Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5))))))
                                                     in ((ap_addCmd newAP2 Cmd_IConst1), StackRef)
translate (TypedExp (Infix "<=" l r) t) ap = let lhs = translate l ap
                                             in let rhs = translate r (pushToStack (fst lhs) (snd lhs))
                                                in let newAP = ap_addCmd (pushToStack (fst rhs) (snd rhs)) (Cmd_IfICmpLe (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 4))))))
                                                   in let newAP2 = ap_addCmd (ap_addCmd newAP Cmd_IConst0) (Cmd_Goto (head (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5)))) (head (tail (fromShort (useValueOf ((ap_getPos (fst rhs)) + 5))))))
                                                      in ((ap_addCmd newAP2 Cmd_IConst1), StackRef)

-- Unary Expressions

translate (TypedExp (Unary "+" x) t) ap = translate x ap
translate (TypedExp (Unary "-" x) t) ap = ((ap_addCmd (pushToStack (fst (translate x ap)) (snd (translate x ap))) Cmd_INeg), StackRef)
translate (TypedExp (Unary "~" x) t) ap = translate (TypedExp (Infix "^" x (TypedExp (Integer (-1)) "int")) t) ap
translate (TypedExp (Unary "!" x) t) ap = translate (TypedExp (Infix "^" x (TypedExp (Integer (1)) "int")) t) ap

translate (TypedExp (ConditionalExp x i e) t) ap = let newAP = pushToStack (fst (translate x ap)) (snd (translate x ap))
                                                   in let fstPos = ap_getPos newAP
                                                      in let newAP1 = pushToStack (fst (translate i (ap_addCmd newAP Srgt_Jump))) (snd (translate i (ap_addCmd newAP Srgt_Jump)))
                                                         in let sndPos = ap_getPos newAP1
                                                            in let newAP2 = pushToStack (fst (translate e (ap_addCmd newAP1 Srgt_Jump))) (snd (translate e (ap_addCmd newAP1 Srgt_Jump)))
                                                               in let newAP3 = ap_setCode newAP2 ((ap_getCode newAP2) !# fstPos # (Cmd_IfEq (head (fromShort (useValueOf (sndPos + 1)))) (head (tail (fromShort (useValueOf (sndPos + 1)))))))
                                                                  in let newAP4 = ap_setCode newAP3 ((ap_getCode newAP3) !# sndPos # (Cmd_Goto (head (fromShort (useValueOf (ap_getPos newAP3)))) (head (tail (fromShort (useValueOf (ap_getPos newAP3)))))))
                                                                     in (newAP4, StackRef)

translate (TypedExp (Cast "int" (TypedExp x "int")) t) ap = translate (TypedExp x "int") ap
translate (TypedExp (Cast "int" (TypedExp x "char")) t) ap = translate (TypedExp x "char") ap
translate (TypedExp (Cast "int" (TypedExp x "bool")) t) ap = translate (TypedExp x "bool") ap
translate (TypedExp (Cast "int" (TypedExp x "short")) t) ap = translate (TypedExp x "short") ap
translate (TypedExp (Cast "char" (TypedExp x "int")) t) ap = ((ap_addCmd (pushToStack (fst (translate (TypedExp x "int") ap)) (snd (translate (TypedExp x "int") ap))) Cmd_I2C), StackRef)
translate (TypedExp (Cast "char" (TypedExp x "char")) t) ap = translate (TypedExp x "char") ap
translate (TypedExp (Cast "char" (TypedExp x "bool")) t) ap = translate (TypedExp x "bool") ap
translate (TypedExp (Cast "char" (TypedExp x "short")) t) ap = translate (TypedExp x "short") ap
translate (TypedExp (Cast "bool" (TypedExp x "int")) t) ap = ((ap_addCmd (pushToStack (fst (translate (TypedExp x "int") ap)) (snd (translate (TypedExp x "int") ap))) Cmd_I2B), StackRef)
translate (TypedExp (Cast "bool" (TypedExp x "char")) t) ap = ((ap_addCmd (pushToStack (fst (translate (TypedExp x "char") ap)) (snd (translate (TypedExp x "char") ap))) Cmd_I2B), StackRef)
translate (TypedExp (Cast "bool" (TypedExp x "bool")) t) ap = translate (TypedExp x "bool") ap
translate (TypedExp (Cast "bool" (TypedExp x "short")) t) ap = ((ap_addCmd (pushToStack (fst (translate (TypedExp x "short") ap)) (snd (translate (TypedExp x "short") ap))) Cmd_I2B), StackRef)
translate (TypedExp (Cast "short" (TypedExp x "int")) t) ap = ((ap_addCmd (pushToStack (fst (translate (TypedExp x "int") ap)) (snd (translate (TypedExp x "int") ap))) Cmd_I2S), StackRef)
translate (TypedExp (Cast "short" (TypedExp x "char")) t) ap = translate (TypedExp x "char") ap
translate (TypedExp (Cast "short" (TypedExp x "bool")) t) ap = translate (TypedExp x "bool") ap
translate (TypedExp (Cast "short" (TypedExp x "short")) t) ap = translate (TypedExp x "short") ap
-- translate (TypedExp (Cast "int" (TypedExp x "long")) t) ap = ((ap_addCmd (pushToStack (fst (translate (TypedExp x "long") ap)) (snd (translate (TypedExp x "long") ap))) Cmd_L2I), StackRef)





translate (TypedExp (StatementExpExp x) t) ap = translateSE x ap

translate (TypedExp x t) y = (y, NullRef)

translate x y = translate (TypedExp x "") y    -- not to be called if everything is typed correctly



-- translate Statement into AbstractProg and return Reference to evaluation

translateS :: Statement -> AbstractProg -> (AbstractProg, Reference)

translateS (LocalVarDecl t n (Just x) True) ap = let ap_r = (translate (TypedExp x t) ap)
                                                 in ((ap_setNamePool (fst ap_r) ((ap_getNamePool (fst ap_r)) !% n % (snd ap_r))), (snd ap_r))
translateS (LocalVarDecl t n Nothing True) ap = ((ap_setNamePool ap ((ap_getNamePool ap) !% n % UndefConst)), UndefConst)
translateS (LocalVarDecl t n (Just x) False) ap = let ap_r = (translate (TypedExp x t) ap)
                                                  in ((popToVar (pushToStack (fst ap_r) (snd ap_r)) n t), (snd ap_r))
translateS (LocalVarDecl "int" n Nothing False) ap = translateS (LocalVarDecl "int" n (Just (TypedExp (Integer 0) "int")) False) ap
-- "float" , "long" , "double" not implemented
translateS (LocalVarDecl reftype n Nothing False) ap = ((popToVar (pushToStack ap NullRef) n reftype), NullRef)

translateS (If x i (Just e)) ap = let newAP = pushToStack (fst (translate x ap)) (snd (translate x ap))
                                  in let fstPos = ap_getPos newAP
                                     in let newAP1 = cleanRef (translateS i (ap_addCmd newAP Srgt_Jump))
                                        in let sndPos = ap_getPos newAP1
                                           in let newAP2 = cleanRef (translateS e (ap_addCmd newAP1 Srgt_Jump))
                                              in let newAP3 = ap_setCode newAP2 ((ap_getCode newAP2) !# fstPos # (Cmd_IfEq (head (fromShort (useValueOf (sndPos + 1)))) (head (tail (fromShort (useValueOf (sndPos + 1)))))))
                                                 in let newAP4 = ap_setCode newAP3 ((ap_getCode newAP3) !# sndPos # (Cmd_Goto (head (fromShort (useValueOf (ap_getPos newAP3)))) (head (tail (fromShort (useValueOf (ap_getPos newAP3)))))))
                                                    in (newAP4, NullRef)
translateS (If x i Nothing) ap = let newAP = pushToStack (fst (translate x ap)) (snd (translate x ap))
                                 in let fstPos = ap_getPos newAP
                                    in let newAP1 = cleanRef (translateS i (ap_addCmd newAP Srgt_Jump))
                                       in let newAP2 = ap_setCode newAP1 ((ap_getCode newAP1) !# fstPos # (Cmd_IfEq (head (fromShort (useValueOf (ap_getPos newAP1)))) (head (tail (fromShort (useValueOf (ap_getPos newAP1)))))))
                                          in (newAP2, NullRef)

translateS (While x b) ap = let fstPos = ap_getPos ap
                            in let newAP = pushToStack (fst (translate x ap)) (snd (translate x ap))
                               in let sndPos = ap_getPos newAP
                                  in let newAP1 = cleanRef (translateS b (ap_addCmd newAP Srgt_Jump))
                                     in let continueJump = Cmd_Goto (head (fromShort (useValueOf fstPos))) (head (tail (fromShort (useValueOf fstPos))))
                                        in let newAP2 = ap_addCmd newAP1 continueJump
                                           in let newAP3 = ap_setCode newAP2 ((ap_getCode newAP2) !# sndPos # (Cmd_IfEq (head (fromShort (useValueOf (ap_getPos newAP2)))) (head (tail (fromShort (useValueOf (ap_getPos newAP2)))))))
                                              in let breakJump = Cmd_Goto (head (fromShort (useValueOf (ap_getPos newAP2)))) (head (tail (fromShort (useValueOf (ap_getPos newAP2)))))
                                                 in let newAP4 = ap_setCode newAP3 (replaceSurrogates_While (ap_getCode newAP3) sndPos (ap_getPos newAP2) continueJump breakJump)
                                                    in (newAP4, NullRef)


translateS (For x y z b) ap = translateS (loopConversion (For x y z b)) ap

translateS (Do b x) ap = let prePos = ap_getPos ap
                         in let oldAP = cleanRef (translateS b ap)
                            in let fstPos = ap_getPos oldAP
                               in let newAP = pushToStack (fst (translate x oldAP)) (snd (translate x oldAP))
                                  in let continueJump = Cmd_Goto (head (fromShort (useValueOf fstPos))) (head (tail (fromShort (useValueOf fstPos))))
                                     in let newAP2 = ap_addCmd newAP (Cmd_IfNe (head (fromShort (useValueOf prePos))) (head (tail (fromShort (useValueOf prePos)))))
                                        in let breakJump = Cmd_Goto (head (fromShort (useValueOf (ap_getPos newAP2)))) (head (tail (fromShort (useValueOf (ap_getPos newAP2)))))
                                           in let newAP3 = ap_setCode newAP2 (replaceSurrogates_While (ap_getCode newAP2) prePos fstPos continueJump breakJump)
                                              in (newAP3, NullRef)


translateS (LabeledStatement n (While x b)) ap = let newAP = ap_setNamePool ap ((ap_getNamePool ap) !% n % (CodeRef (ap_getPos ap)))
                                                 in let ap_r = translateS (While x b) newAP
                                                    in ((ap_setNamePool (fst ap_r) ((ap_getNamePool (fst ap_r)) !% (n ++ "/") % (CodeRef (ap_getPos (fst ap_r))))), (snd ap_r))
translateS (LabeledStatement n (For x y z b)) ap = translateS (loopConversion (LabeledStatement n (For x y z b))) ap
translateS (LabeledStatement n (Do b x)) ap = let continuePos = ap_getPos (fst (translateS b ap))
                                              in let newAP = ap_setNamePool ap ((ap_getNamePool ap) !% n % (CodeRef continuePos))
                                                 in let ap_r = translateS (Do b x) newAP
                                                    in ((ap_setNamePool (fst ap_r) ((ap_getNamePool (fst ap_r)) !% (n ++ "/") % (CodeRef (ap_getPos (fst ap_r))))), (snd ap_r))
translateS (LabeledStatement n x) ap = let newAP = ap_setNamePool ap ((ap_getNamePool ap) !% n % (CodeRef (ap_getPos ap)))
                                       in translateS x newAP



translateS (Block []) ap = (ap, NullRef)
translateS (Block (s : ss)) ap = translateS (Block (ss)) (cleanRef (translateS s ap))

translateS (Break (Just n)) ap = (ap_addCmd ap (Srgt_GotoLabel (n ++ "/")), NullRef)
translateS (Break Nothing) ap = (ap_addCmd ap Srgt_BreakLoop, NullRef)

translateS (Continue (Just n)) ap = (ap_addCmd ap (Srgt_GotoLabel n), NullRef)
translateS (Continue Nothing) ap = (ap_addCmd ap Srgt_Continue, NullRef)

translateS EmptyStatement ap = (ap, NullRef)
----

--

translateS (StatementExpStatement x) ap = translateSE x ap

translateS x y = (y, NullRef)




-- translate StatementExp into AbstractProg and return Reference to evaluation

translateSE :: StatementExp -> AbstractProg -> (AbstractProg, Reference)

translateSE (TypedStatementExp (TypedStatementExp x tt) t) ap = translateSE (TypedStatementExp x tt) ap

translateSE (TypedStatementExp (Assign l r "=") t) ap = let lhs = translate (TypedExp l t) ap
                                                        in let rhs = translate (TypedExp r t) (fst lhs)
                                                           in ((writeToRef (pushToStack (fst rhs) (snd rhs)) "" (snd lhs)), (snd lhs))
translateSE (TypedStatementExp (Assign l r "+=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "+" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "*=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "*" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "-=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "-" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "/=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "/" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "%=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "%" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "<<=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "<<" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r ">>=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix ">>" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r ">>>=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix ">>>" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "&=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "&" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "|=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "|" l r) t) "=") t) ap
translateSE (TypedStatementExp (Assign l r "^=") t) ap = translateSE (TypedStatementExp (Assign l (TypedExp (Infix "^" l r) t) "=") t) ap

translateSE (TypedStatementExp (PostfixUnary "++" x) "int") ap = let ap_r = translate x ap
                                                                 in let makeIInc (RefToV_I r) | (r < 256) = Cmd_IInc (useValueOf r) 1
                                                                                              | otherwise = Cmd_Wide_IInc (head (fromShort (useValueOf r))) (head (tail (fromShort (useValueOf r)))) 0 1
                                                                    in (ap_addCmd (pushToStack (fst ap_r) (snd ap_r)) (makeIInc (snd ap_r)), StackRef)
translateSE (TypedStatementExp (PostfixUnary "--" x) "int") ap = let ap_r = translate x ap
                                                                 in let makeIInc (RefToV_I r) | (r < 256) = Cmd_IInc (useValueOf r) (-1)
                                                                                              | otherwise = Cmd_Wide_IInc (head (fromShort (useValueOf r))) (head (tail (fromShort (useValueOf r)))) 0 (-1)
                                                                    in (ap_addCmd (pushToStack (fst ap_r) (snd ap_r)) (makeIInc (snd ap_r)), StackRef)

translateSE (TypedStatementExp (PrefixUnary "++" x) "int") ap = let ap_r = translate x ap
                                                                 in let makeIInc (RefToV_I r) | (r < 256) = Cmd_IInc (useValueOf r) 1
                                                                                              | otherwise = Cmd_Wide_IInc (head (fromShort (useValueOf r))) (head (tail (fromShort (useValueOf r)))) 0 1
                                                                    in (ap_addCmd (fst ap_r) (makeIInc (snd ap_r)), (snd ap_r))
translateSE (TypedStatementExp (PrefixUnary "--" x) "int") ap = let ap_r = translate x ap
                                                                 in let makeIInc (RefToV_I r) | (r < 256) = Cmd_IInc (useValueOf r) (-1)
                                                                                              | otherwise = Cmd_Wide_IInc (head (fromShort (useValueOf r))) (head (tail (fromShort (useValueOf r)))) 0 (-1)
                                                                    in (ap_addCmd (fst ap_r) (makeIInc (snd ap_r)), (snd ap_r))



-- Stack should be popped later, either in Assignment or seperately

translateSE (TypedStatementExp x t) y = (y, NullRef)

translateSE x y = translateSE (TypedStatementExp x "") y    -- not to be called if everything is typed correctly



-- Benjamin Hellstern, Magdalena Sannwald
-- Seminar Compilerbau WS 14/15
-- WSI Informatik, Ernst-Bloch-Universität Tübingen