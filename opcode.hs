module OpCode where

import ByteMath
import Indexing
import Tagging
import Data.Word
import qualified Data.ByteString.Lazy as B

-- http://en.wikipedia.org/wiki/Java_bytecode_instruction_listings

-- http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-3.html#jvms-3.10
-- https://docs.oracle.com/javase/specs/jvms/se5.0/html/Instructions2.doc14.html

-- https://hackage.haskell.org/package/bytestring-0.9.2.1/docs/Data-ByteString.html
-- http://hackage.haskell.org/package/bytestring-0.9.0.4/docs/Data-ByteString-Lazy.html

type Arg = Word8     -- Byte Argument
type Args = [Arg]    -- Byte Argument List
data Padding = PaddingTo4 deriving (Show)    -- 0 to 3 null bytes


data Command = Explicit Arg              -- Byte Argument
             | Cmd_Nop                   -- No Operation
             | Cmd_AConstNull            -- Null Pointer onto Stack
             | Cmd_IConstM1              -- Int -1 onto Stack
             | Cmd_IConst0               -- Int 0 onto Stack
             | Cmd_IConst1               -- Int 1 onto Stack
             | Cmd_IConst2               -- Int 2 onto Stack
             | Cmd_IConst3               -- Int 3 onto Stack
             | Cmd_IConst4               -- Int 4 onto Stack
             | Cmd_IConst5               -- Int 5 onto Stack
             | Cmd_LConst0               -- Long 0 onto Stack
             | Cmd_LConst1               -- Long 1 onto Stack
             | Cmd_FConst0               -- Float 0.0 onto Stack
             | Cmd_FConst1               -- Float 1.0 onto Stack
             | Cmd_FConst2               -- Float 2.0 onto Stack
             | Cmd_DConst0               -- Double 0.0 onto Stack
             | Cmd_DConst1               -- Double 1.0 onto Stack
             | Cmd_BIPush Arg            -- Arg (Byte -> Int) onto Stack
             | Cmd_SIPush Arg Arg        -- Args (Short -> Int) onto Stack
             | Cmd_LdC_I Arg             -- Arg (Index -> Constant String/Int/Float) onto Stack
             | Cmd_LdC_F Arg
             | Cmd_LdC_S Arg
             | Cmd_LdCW_I Arg Arg        -- Args (Index -> Constant String/Int/Float) onto Stack
             | Cmd_LdCW_F Arg Arg
             | Cmd_LdCW_S Arg Arg
             | Cmd_LdC2W_L Arg Arg       -- Args (Index -> Constant Long/Double) onto Stack
             | Cmd_LdC2W_D Arg Arg
             | Cmd_ILoad Arg             -- Arg (Index -> Int) onto Stack
             | Cmd_LLoad Arg             -- Arg (Index -> Long) onto Stack
             | Cmd_FLoad Arg             -- Arg (Index -> Float) onto Stack
             | Cmd_DLoad Arg             -- Arg (Index -> Double) onto Stack
             | Cmd_ALoad Arg             -- Arg (Index -> Pointer) onto Stack
             | Cmd_ILoad0                -- (LocalVar 0 -> Int) onto Stack
             | Cmd_ILoad1                -- (LocalVar 1 -> Int) onto Stack
             | Cmd_ILoad2                -- (LocalVar 2 -> Int) onto Stack
             | Cmd_ILoad3                -- (LocalVar 3 -> Int) onto Stack
             | Cmd_LLoad0                -- (LocalVar 0 -> Long) onto Stack
             | Cmd_LLoad1                -- (LocalVar 1 -> Long) onto Stack
             | Cmd_LLoad2                -- (LocalVar 2 -> Long) onto Stack
             | Cmd_LLoad3                -- (LocalVar 3 -> Long) onto Stack
             | Cmd_FLoad0                -- (LocalVar 0 -> Float) onto Stack
             | Cmd_FLoad1                -- (LocalVar 1 -> Float) onto Stack
             | Cmd_FLoad2                -- (LocalVar 2 -> Float) onto Stack
             | Cmd_FLoad3                -- (LocalVar 3 -> Float) onto Stack
             | Cmd_DLoad0                -- (LocalVar 0 -> Double) onto Stack
             | Cmd_DLoad1                -- (LocalVar 1 -> Double) onto Stack
             | Cmd_DLoad2                -- (LocalVar 2 -> Double) onto Stack
             | Cmd_DLoad3                -- (LocalVar 3 -> Double) onto Stack
             | Cmd_ALoad0                -- (LocalVar 0 -> Pointer) onto Stack
             | Cmd_ALoad1                -- (LocalVar 1 -> Pointer) onto Stack
             | Cmd_ALoad2                -- (LocalVar 2 -> Pointer) onto Stack
             | Cmd_ALoad3                -- (LocalVar 3 -> Pointer) onto Stack
             | Cmd_IALoad                -- (Int from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_LALoad                -- (Long from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_FALoad                -- (Float from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_DALoad                -- (Double from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_AALoad                -- (Pointer from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_BALoad                -- (Byte from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_CALoad                -- (Char from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_SALoad                -- (Short from Array (from Stack) at Index (from Stack)) onto Stack
             | Cmd_IStore Arg            -- Int from Stack into Var (at Index)
             | Cmd_LStore Arg            -- Long from Stack into Var (at Index)
             | Cmd_FStore Arg            -- Float from Stack into Var (at Index)
             | Cmd_DStore Arg            -- Double from Stack into Var (at Index)
             | Cmd_AStore Arg            -- Pointer from Stack into Var (at Index)
             | Cmd_IStore0               -- Int from Stack into Var0
             | Cmd_IStore1               -- Int from Stack into Var1
             | Cmd_IStore2               -- Int from Stack into Var2
             | Cmd_IStore3               -- Int from Stack into Var3
             | Cmd_LStore0               -- Long from Stack into Var0
             | Cmd_LStore1               -- Long from Stack into Var1
             | Cmd_LStore2               -- Long from Stack into Var2
             | Cmd_LStore3               -- Long from Stack into Var3
             | Cmd_FStore0               -- Float from Stack into Var0
             | Cmd_FStore1               -- Float from Stack into Var1
             | Cmd_FStore2               -- Float from Stack into Var2
             | Cmd_FStore3               -- Float from Stack into Var3
             | Cmd_DStore0               -- Double from Stack into Var0
             | Cmd_DStore1               -- Double from Stack into Var1
             | Cmd_DStore2               -- Double from Stack into Var2
             | Cmd_DStore3               -- Double from Stack into Var3
             | Cmd_AStore0               -- Pointer from Stack into Var0
             | Cmd_AStore1               -- Pointer from Stack into Var1
             | Cmd_AStore2               -- Pointer from Stack into Var2
             | Cmd_AStore3               -- Pointer from Stack into Var3
             | Cmd_IAStore               -- Store into Array (from Stack) at Index (from Stack) an Int (from Stack)
             | Cmd_LAStore               -- Store into Array (from Stack) at Index (from Stack) a Long (from Stack)
             | Cmd_FAStore               -- Store into Array (from Stack) at Index (from Stack) a Float (from Stack)
             | Cmd_DAStore               -- Store into Array (from Stack) at Index (from Stack) a Double (from Stack)
             | Cmd_AAStore               -- Store into Array (from Stack) at Index (from Stack) a Pointer (from Stack)
             | Cmd_BAStore               -- Store into Array (from Stack) at Index (from Stack) a Byte (from Stack)
             | Cmd_CAStore               -- Store into Array (from Stack) at Index (from Stack) a Char (from Stack)
             | Cmd_SAStore               -- Store into Array (from Stack) at Index (from Stack) a Short (from Stack)
             | Cmd_Pop                   -- Pop (Int, Float, Pointer) from Stack
             | Cmd_Pop2                  -- Pop (Long, Double) from Stack , or Pop two of (Int, Float, Pointer) from Stack
             | Cmd_Dup                   -- Duplicate topmost (Int, Float, Pointer) from Stack to top
             | Cmd_DupX1                 -- Duplicate topmost (Int, Float, Pointer) from Stack to two places (skip one Int) from top
             | Cmd_DupX2                 -- Duplicate topmost (Long, Double) from Stack to four places (skip one Double) from top
             | Cmd_Dup2                  -- Duplicate topmost (Long, Double) from Stack to top , or topmost two (Int, Float, Pointer)
             | Cmd_Dup2X1                -- Duplicate topmost (Long, Double) from Stack to four places (skip one Int) from top , or topmost two (Int, Float, Pointer)
             | Cmd_Dup2X2                -- Duplicate topmost (Long, Double) from Stack to five places (skip one Double) from top , or topmost two (Int, Float, Pointer)
             | Cmd_Swap                  -- Swap topmost two (Int, Float, Pointer) on Stack
             | Cmd_IAdd                  -- Add two Int from Stack , result onto Stack
             | Cmd_LAdd                  -- Add two Long from Stack , result onto Stack
             | Cmd_FAdd                  -- Add two Float from Stack , result onto Stack
             | Cmd_DAdd                  -- Add two Double from Stack , result onto Stack
             | Cmd_ISub                  -- Subtract two Int from Stack , result onto Stack
             | Cmd_LSub                  -- Subtract two Long from Stack , result onto Stack
             | Cmd_FSub                  -- Subtract two Float from Stack , result onto Stack
             | Cmd_DSub                  -- Subtract two Double from Stack , result onto Stack
             | Cmd_IMul                  -- Multiply two Int from Stack , result onto Stack
             | Cmd_LMul                  -- Multiply two Long from Stack , result onto Stack
             | Cmd_FMul                  -- Multiply two Float from Stack , result onto Stack
             | Cmd_DMul                  -- Multiply two Double from Stack , result onto Stack
             | Cmd_IDiv                  -- Divide two Int from Stack , result onto Stack
             | Cmd_LDiv                  -- Divide two Long from Stack , result onto Stack
             | Cmd_FDiv                  -- Divide two Float from Stack , result onto Stack
             | Cmd_DDiv                  -- Divide two Double from Stack , result onto Stack
             | Cmd_IRem                  -- Modulo of two Int from Stack , result onto Stack
             | Cmd_LRem                  -- Modulo of two Long from Stack , result onto Stack
             | Cmd_FRem                  -- Modulo of two Float from Stack , result onto Stack
             | Cmd_DRem                  -- Modulo of two Double from Stack , result onto Stack
             | Cmd_INeg                  -- Negative of Int from Stack , result onto Stack
             | Cmd_LNeg                  -- Negative of Long from Stack , result onto Stack
             | Cmd_FNeg                  -- Negative of Float from Stack , result onto Stack
             | Cmd_DNeg                  -- Negative of Double from Stack , result onto Stack
             | Cmd_IShL                  -- L-Shift Int from Stack , result onto Stack
             | Cmd_LShL                  -- L-Shift Long from Stack , result onto Stack
             | Cmd_IShR                  -- R-Shift Int from Stack , result onto Stack
             | Cmd_LShR                  -- R-Shift Long from Stack , result onto Stack
             | Cmd_IUShR                 -- Logical R-Shift Int from Stack , result onto Stack
             | Cmd_LUShR                 -- Logical R-Shift Long from Stack , result onto Stack
             | Cmd_IAnd                  -- Bitwise And of two Int from Stack , result onto Stack
             | Cmd_LAnd                  -- Bitwise And of two Long from Stack , result onto Stack
             | Cmd_IOr                   -- Bitwise Or of two Int from Stack , result onto Stack
             | Cmd_LOr                   -- Bitwise Or of two Long from Stack , result onto Stack
             | Cmd_IXor                  -- Bitwise Xor of two Int from Stack , result onto Stack
             | Cmd_LXor                  -- Bitwise Xor of two Long from Stack , result onto Stack
             | Cmd_IInc Arg Arg          -- Increment LocalVar (at Index) by Constant (Byte-Arg)
             | Cmd_I2L                   -- Convert topmost Stack-Value from Int to Long
             | Cmd_I2F                   -- Convert topmost Stack-Value from Int to Float
             | Cmd_I2D                   -- Convert topmost Stack-Value from Int to Double
             | Cmd_L2I                   -- Convert topmost Stack-Values from Long to Int
             | Cmd_L2F                   -- Convert topmost Stack-Values from Long to Float
             | Cmd_L2D                   -- Convert topmost Stack-Values from Long to Double
             | Cmd_F2I                   -- Convert topmost Stack-Value from Float to Int
             | Cmd_F2L                   -- Convert topmost Stack-Value from Float to Long
             | Cmd_F2D                   -- Convert topmost Stack-Value from Float to Double
             | Cmd_D2I                   -- Convert topmost Stack-Values from Double to Int
             | Cmd_D2L                   -- Convert topmost Stack-Values from Double to Long
             | Cmd_D2F                   -- Convert topmost Stack-Values from Double to Float
             | Cmd_I2B                   -- Convert topmost Stack-Value from Int to Byte
             | Cmd_I2C                   -- Convert topmost Stack-Value from Int to Char
             | Cmd_I2S                   -- Convert topmost Stack-Value from Int to Short
             | Cmd_LCmp                  -- Compare topmost two Long from Stack , result to Stack
             | Cmd_FCmpL                 -- Compare topmost two Float from Stack , result to Stack
             | Cmd_FCmpG                 -- Compare topmost two Float from Stack , result to Stack
             | Cmd_DCmpL                 -- Compare topmost two Double from Stack , result to Stack
             | Cmd_DCmpG                 -- Compare topmost two Double from Stack , result to Stack
             | Cmd_IfEq Arg Arg          -- If topmost Value = 0 : jump to offset (Args)
             | Cmd_IfNe Arg Arg          -- If topmost Value != 0 : jump to offset (Args)
             | Cmd_IfLt Arg Arg          -- If topmost Value < 0 : jump to offset (Args)
             | Cmd_IfGe Arg Arg          -- If topmost Value >= 0 : jump to offset (Args)
             | Cmd_IfGt Arg Arg          -- If topmost Value > 0 : jump to offset (Args)
             | Cmd_IfLe Arg Arg          -- If topmost Value <= 0 : jump to offset (Args)
             | Cmd_IfICmpEq Arg Arg      -- If topmost two Int (from Stack) are equal : jump to offset (Args)
             | Cmd_IfICmpNe Arg Arg      -- If topmost two Int (from Stack) are not equal : jump to offset (Args)
             | Cmd_IfICmpLt Arg Arg      -- If topmost Int (from Stack) < second topmost Int (from Stack) : jump to offset (Args)
             | Cmd_IfICmpGe Arg Arg      -- If topmost Int (from Stack) >= second topmost Int (from Stack) : jump to offset (Args)
             | Cmd_IfICmpGt Arg Arg      -- If topmost Int (from Stack) > second topmost Int (from Stack) : jump to offset (Args)
             | Cmd_IfICmpLe Arg Arg      -- If topmost Int (from Stack) > second topmost Int (from Stack) : jump to offset (Args)
             | Cmd_IfACmpEq Arg Arg      -- If topmost two Pointer (from Stack) are equal : jump to offset (Args)
             | Cmd_IfACmpNe Arg Arg      -- If topmost two Pointer (from Stack) are not equal : jump to offset (Args)
             | Cmd_Goto Arg Arg          -- Jump to offset (Args)
             | Cmd_JSr Arg Arg           -- Jump to subroutine offset (Args) , push Return-Address onto Stack
             | Cmd_Ret Arg               -- continue at address (from Lacalvar at Index Arg)
--
             | Cmd_TableSwitch Padding Arg Arg Arg Arg Arg Arg Arg Arg Arg Arg Arg Arg Args      -- Switch with default (Args 1-4) , lower Bound (Args 5-8) and upper Bound (Args 9-12) , jump to Addresses in Args-Array
             | Cmd_LookupSwitch Padding Arg Arg Arg Arg Arg Arg Arg Arg Args                     -- Switch with default (Args 1-4) and number (Args 5-8) of pairs (in Args-Array) of Index and Address
--
             | Cmd_IReturn               -- return Int (from Stack) from Method
             | Cmd_LReturn               -- return Long (from Stack) from Method
             | Cmd_FReturn               -- return Float (from Stack) from Method
             | Cmd_DReturn               -- return Double (from Stack) from Method
             | Cmd_AReturn               -- return Pointer (from Stack) from Method
             | Cmd_Return                -- return Void from Method
             | Cmd_GetStatic Arg Arg     -- push Value of Static Field (at Index) onto Stack
             | Cmd_PutStatic Arg Arg     -- set Value (from Stack) to Static Field (at Index)
             | Cmd_GetField Arg Arg      -- push Value of Field (at Index Arg2 within Object (Reference Arg1)) onto Stack
             | Cmd_PutField Arg Arg      -- set Value (from Stack) to Field (at Index Arg2 within Object (Reference Arg1))
             | Cmd_InvokeV Arg Arg       -- Virtual Method Call on object (Pointer from Stack) at Index (Args)
             | Cmd_InvokeSp Arg Arg      -- Method Call on object (Pointer from Stack) at Index (Args)
             | Cmd_InvokeSt Arg Arg      -- Static Method Call at Index (Args)
             | Cmd_InvokeIF Arg Arg      -- Interface Method Call on object (Pointer from Stack) at Index (Args)
             | Cmd_InvokeD Arg Arg       -- Dynamic Method Call at Index (Args)
             | Cmd_New Arg Arg           -- push Pointer to new object of Type at Index (Args) onto Stack
             | Cmd_NewArray Arg          -- Create new Array on Stack with Length (from Stack) and Type (referenced by Arg)
             | Cmd_ANewArray Arg Arg     -- Create new Array on Stack with Length (from Stack) and Type (referenced by Args)
             | Cmd_ArrayLenght           -- return length of Array (Pointer from Stack)
             | Cmd_AThrow                -- clear Stack, throw Exception, Pointer to Sender onto Stack
             | Cmd_CheckCast Arg Arg     -- check whether Pointer (topmost on Stack) is of Type at Index (Args)
             | Cmd_InstanceOf Arg Arg    -- check whether Pointer (from Stack) is of Type at Index (Args) , push result
             | Cmd_MonitorEnter          -- lock Objekt (Pointer from Stack)
             | Cmd_MonitorExit           -- unlock Objekt (Pointer from Stack)
--
-- wide
             | Cmd_Wide_ILoad Arg Arg             -- Arg (Index -> Int) onto Stack
             | Cmd_Wide_LLoad Arg Arg             -- Arg (Index -> Long) onto Stack
             | Cmd_Wide_FLoad Arg Arg             -- Arg (Index -> Float) onto Stack
             | Cmd_Wide_DLoad Arg Arg             -- Arg (Index -> Double) onto Stack
             | Cmd_Wide_ALoad Arg Arg             -- Arg (Index -> Pointer) onto Stack
             | Cmd_Wide_IStore Arg Arg            -- Int from Stack into Var (at Index)
             | Cmd_Wide_LStore Arg Arg            -- Long from Stack into Var (at Index)
             | Cmd_Wide_FStore Arg Arg            -- Float from Stack into Var (at Index)
             | Cmd_Wide_DStore Arg Arg            -- Double from Stack into Var (at Index)
             | Cmd_Wide_AStore Arg Arg            -- Pointer from Stack into Var (at Index)
             | Cmd_Wide_Ret Arg Arg               -- continue at address (from LocalVar at Index Arg)
             | Cmd_Wide_IInc Arg Arg Arg Arg      -- Increment LocalVar (at Index Args 1 and 2) by Constant (Args 3 and 4)
--
             | Cmd_MultiANewArray Arg Arg Arg     -- Create new Array on Stack with Length (from Stack) and Type (referenced by Args 1 and 2) and Dimensions (Arg 3)
             | Cmd_IfNull Arg Arg                 -- If topmost Value = NULL : jump to offset (Args)
             | Cmd_IfNonNull Arg Arg              -- If topmost Value != NULL : jump to offset (Args)
             | Cmd_GotoW Arg Arg Arg Arg          -- Jump to offset (Args)
             | Cmd_JSrW Arg Arg Arg Arg           -- Jump to subroutine offset (Args) , push Return-Address onto Stack
--
-- debug
             | Breakpoint  -- Dubugging only, not to be used !
             | ImpDep1     -- Dubugging only, not to be used !
             | ImpDep2     -- Dubugging only, not to be used !
--
             deriving (Show)



-- Extract Arguments from Commands (to be used exactly once before Serialization !!!)

extractArgs :: [(Command, Int)] -> [Command]

extractArgs((Cmd_BIPush arg, offset) : xs) = ((Cmd_BIPush arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_SIPush arg1 arg2, offset) : xs) = ((Cmd_SIPush arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_LdC_I arg, offset) : xs) = ((Cmd_LdC_I arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_LdC_F arg, offset) : xs) = ((Cmd_LdC_F arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_LdC_S arg, offset) : xs) = ((Cmd_LdC_S arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_LdCW_I arg1 arg2, offset) : xs) = ((Cmd_LdCW_I arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_LdCW_F arg1 arg2, offset) : xs) = ((Cmd_LdCW_F arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_LdCW_S arg1 arg2, offset) : xs) = ((Cmd_LdCW_S arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_LdC2W_L arg1 arg2, offset) : xs) = ((Cmd_LdC2W_L arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_LdC2W_D arg1 arg2, offset) : xs) = ((Cmd_LdC2W_D arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_ILoad arg, offset) : xs) = ((Cmd_ILoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_LLoad arg, offset) : xs) = ((Cmd_LLoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_FLoad arg, offset) : xs) = ((Cmd_FLoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_DLoad arg, offset) : xs) = ((Cmd_DLoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_ALoad arg, offset) : xs) = ((Cmd_ALoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_IStore arg, offset) : xs) = ((Cmd_ILoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_LStore arg, offset) : xs) = ((Cmd_LLoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_FStore arg, offset) : xs) = ((Cmd_FLoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_DStore arg, offset) : xs) = ((Cmd_DLoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_AStore arg, offset) : xs) = ((Cmd_ALoad arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_IInc arg1 arg2, offset) : xs) = ((Cmd_IInc arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfEq arg1 arg2, offset) : xs) = ((Cmd_IfEq arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfNe arg1 arg2, offset) : xs) = ((Cmd_IfNe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfLt arg1 arg2, offset) : xs) = ((Cmd_IfLt arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfGe arg1 arg2, offset) : xs) = ((Cmd_IfGe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfGt arg1 arg2, offset) : xs) = ((Cmd_IfGt arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfLe arg1 arg2, offset) : xs) = ((Cmd_IfLe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfICmpEq arg1 arg2, offset) : xs) = ((Cmd_IfICmpEq arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfICmpNe arg1 arg2, offset) : xs) = ((Cmd_IfICmpNe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfICmpLt arg1 arg2, offset) : xs) = ((Cmd_IfICmpLt arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfICmpGe arg1 arg2, offset) : xs) = ((Cmd_IfICmpGe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfICmpGt arg1 arg2, offset) : xs) = ((Cmd_IfICmpGt arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfICmpLe arg1 arg2, offset) : xs) = ((Cmd_IfICmpLe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfACmpEq arg1 arg2, offset) : xs) = ((Cmd_IfACmpEq arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfACmpNe arg1 arg2, offset) : xs) = ((Cmd_IfACmpNe arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Goto arg1 arg2, offset) : xs) = ((Cmd_Goto arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_JSr arg1 arg2, offset) : xs) = ((Cmd_JSr arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Ret arg, offset) : xs) = ((Cmd_Ret arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args, offset) : xs) | ((rem offset 4) == 3) = ((Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args) : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : (Explicit arg9 : (Explicit argA : (Explicit argB : (Explicit argC : ((map (Explicit ) args) ++ extractArgs(xs)))))))))))))))
                                                                                                                     | ((rem offset 4) == 2) = ((Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args) : (Cmd_Nop : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : (Explicit arg9 : (Explicit argA : (Explicit argB : (Explicit argC : ((map (Explicit ) args) ++ extractArgs(xs))))))))))))))))
                                                                                                                     | ((rem offset 4) == 1) = ((Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args) : (Cmd_Nop : (Cmd_Nop : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : (Explicit arg9 : (Explicit argA : (Explicit argB : (Explicit argC : ((map (Explicit ) args) ++ extractArgs(xs)))))))))))))))))
                                                                                                                     | otherwise = ((Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args) : (Cmd_Nop : (Cmd_Nop : (Cmd_Nop : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : (Explicit arg9 : (Explicit argA : (Explicit argB : (Explicit argC : ((map (Explicit ) args) ++ extractArgs(xs))))))))))))))))))
extractArgs((Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args, offset) : xs) | ((rem offset 4) == 3) = ((Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args) : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : ((map (Explicit ) args) ++ extractArgs(xs)))))))))))
                                                                                                  | ((rem offset 4) == 2) = ((Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args) : (Cmd_Nop : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : ((map (Explicit ) args) ++ extractArgs(xs))))))))))))
                                                                                                  | ((rem offset 4) == 1) = ((Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args) : (Cmd_Nop : (Cmd_Nop : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : ((map (Explicit ) args) ++ extractArgs(xs)))))))))))))
                                                                                                  | otherwise = ((Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args) : (Cmd_Nop : (Cmd_Nop : (Cmd_Nop : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : (Explicit arg5 : (Explicit arg6 : (Explicit arg7 : (Explicit arg8 : ((map (Explicit ) args) ++ extractArgs(xs))))))))))))))
extractArgs((Cmd_GetStatic arg1 arg2, offset) : xs) = ((Cmd_GetStatic arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_PutStatic arg1 arg2, offset) : xs) = ((Cmd_PutStatic arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_GetField arg1 arg2, offset) : xs) = ((Cmd_GetField arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_PutField arg1 arg2, offset) : xs) = ((Cmd_PutField arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_InvokeV arg1 arg2, offset) : xs) = ((Cmd_InvokeV arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_InvokeSp arg1 arg2, offset) : xs) = ((Cmd_InvokeSp arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_InvokeSt arg1 arg2, offset) : xs) = ((Cmd_InvokeSt arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_InvokeIF arg1 arg2, offset) : xs) = ((Cmd_InvokeIF arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_InvokeD arg1 arg2, offset) : xs) = ((Cmd_InvokeD arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_New arg1 arg2, offset) : xs) = ((Cmd_New arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_NewArray arg, offset) : xs) = ((Cmd_NewArray arg) : (Explicit arg : extractArgs(xs)))
extractArgs((Cmd_ANewArray arg1 arg2, offset) : xs) = ((Cmd_ANewArray arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_CheckCast arg1 arg2, offset) : xs) = ((Cmd_CheckCast arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_InstanceOf arg1 arg2, offset) : xs) = ((Cmd_InstanceOf arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_ILoad arg1 arg2, offset) : xs) = ((Cmd_Wide_ILoad arg1 arg2) : (Cmd_ILoad arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_LLoad arg1 arg2, offset) : xs) = ((Cmd_Wide_LLoad arg1 arg2) : (Cmd_LLoad arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_FLoad arg1 arg2, offset) : xs) = ((Cmd_Wide_FLoad arg1 arg2) : (Cmd_FLoad arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_DLoad arg1 arg2, offset) : xs) = ((Cmd_Wide_DLoad arg1 arg2) : (Cmd_DLoad arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_ALoad arg1 arg2, offset) : xs) = ((Cmd_Wide_ALoad arg1 arg2) : (Cmd_ALoad arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_IStore arg1 arg2, offset) : xs) = ((Cmd_Wide_IStore arg1 arg2) : (Cmd_IStore arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_LStore arg1 arg2, offset) : xs) = ((Cmd_Wide_LStore arg1 arg2) : (Cmd_LStore arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_FStore arg1 arg2, offset) : xs) = ((Cmd_Wide_FStore arg1 arg2) : (Cmd_FStore arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_DStore arg1 arg2, offset) : xs) = ((Cmd_Wide_DStore arg1 arg2) : (Cmd_DStore arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_AStore arg1 arg2, offset) : xs) = ((Cmd_Wide_AStore arg1 arg2) : (Cmd_AStore arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_Ret arg1 arg2, offset) : xs) = ((Cmd_Wide_Ret arg1 arg2) : (Cmd_Ret arg1) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_Wide_IInc arg1 arg2 arg3 arg4, offset) : xs) = ((Cmd_Wide_IInc arg1 arg2 arg3 arg4) : (Cmd_IInc arg1 arg2) : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : extractArgs(xs))))))
extractArgs((Cmd_MultiANewArray arg1 arg2 arg3, offset) : xs) = ((Cmd_MultiANewArray arg1 arg2 arg3) : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : extractArgs(xs)))))
extractArgs((Cmd_IfNull arg1 arg2, offset) : xs) = ((Cmd_IfNull arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_IfNonNull arg1 arg2, offset) : xs) = ((Cmd_IfNonNull arg1 arg2) : (Explicit arg1 : (Explicit arg2 : extractArgs(xs))))
extractArgs((Cmd_GotoW arg1 arg2 arg3 arg4, offset) : xs) = ((Cmd_GotoW arg1 arg2 arg3 arg4) : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : extractArgs(xs))))))
extractArgs((Cmd_JSrW arg1 arg2 arg3 arg4, offset) : xs) = ((Cmd_JSrW arg1 arg2 arg3 arg4) : (Explicit arg1 : (Explicit arg2 : (Explicit arg3 : (Explicit arg4 : extractArgs(xs))))))
extractArgs((x, offset) : xs) = (x : extractArgs(xs))
extractArgs([]) = []



-- Serialization to Byte Code

serialize :: Command -> Word8

serialize(Explicit arg) = arg
serialize(Cmd_Nop) = 0x00
serialize(Cmd_AConstNull) = 0x01
serialize(Cmd_IConstM1) = 0x02
serialize(Cmd_IConst0) = 0x03
serialize(Cmd_IConst1) = 0x04
serialize(Cmd_IConst2) = 0x05
serialize(Cmd_IConst3) = 0x06
serialize(Cmd_IConst4) = 0x07
serialize(Cmd_IConst5) = 0x08
serialize(Cmd_LConst0) = 0x09
serialize(Cmd_LConst1) = 0x0a
serialize(Cmd_FConst0) = 0x0b
serialize(Cmd_FConst1) = 0x0c
serialize(Cmd_FConst2) = 0x0d
serialize(Cmd_DConst0) = 0x0e
serialize(Cmd_DConst1) = 0x0f
serialize(Cmd_BIPush arg) = 0x10
serialize(Cmd_SIPush arg1 arg2) = 0x11
serialize(Cmd_LdC_I arg) = 0x12
serialize(Cmd_LdC_F arg) = 0x12
serialize(Cmd_LdC_S arg) = 0x12
serialize(Cmd_LdCW_I arg1 arg2) = 0x13
serialize(Cmd_LdCW_F arg1 arg2) = 0x13
serialize(Cmd_LdCW_S arg1 arg2) = 0x13
serialize(Cmd_LdC2W_L arg1 arg2) = 0x14
serialize(Cmd_LdC2W_D arg1 arg2) = 0x14
serialize(Cmd_ILoad arg) = 0x15
serialize(Cmd_LLoad arg) = 0x16
serialize(Cmd_FLoad arg) = 0x17
serialize(Cmd_DLoad arg) = 0x18
serialize(Cmd_ALoad arg) = 0x19
serialize(Cmd_ILoad0) = 0x1a
serialize(Cmd_ILoad1) = 0x1b
serialize(Cmd_ILoad2) = 0x1c
serialize(Cmd_ILoad3) = 0x1d
serialize(Cmd_LLoad0) = 0x1e
serialize(Cmd_LLoad1) = 0x1f
serialize(Cmd_LLoad2) = 0x20
serialize(Cmd_LLoad3) = 0x21
serialize(Cmd_FLoad0) = 0x22
serialize(Cmd_FLoad1) = 0x23
serialize(Cmd_FLoad2) = 0x24
serialize(Cmd_FLoad3) = 0x25
serialize(Cmd_DLoad0) = 0x26
serialize(Cmd_DLoad1) = 0x27
serialize(Cmd_DLoad2) = 0x28
serialize(Cmd_DLoad3) = 0x29
serialize(Cmd_ALoad0) = 0x2a
serialize(Cmd_ALoad1) = 0x2b
serialize(Cmd_ALoad2) = 0x2c
serialize(Cmd_ALoad3) = 0x2d
serialize(Cmd_IALoad) = 0x2e
serialize(Cmd_LALoad) = 0x2f
serialize(Cmd_FALoad) = 0x30
serialize(Cmd_DALoad) = 0x31
serialize(Cmd_AALoad) = 0x32
serialize(Cmd_BALoad) = 0x33
serialize(Cmd_CALoad) = 0x34
serialize(Cmd_SALoad) = 0x35
serialize(Cmd_IStore arg) = 0x36
serialize(Cmd_LStore arg) = 0x37
serialize(Cmd_FStore arg) = 0x38
serialize(Cmd_DStore arg) = 0x39
serialize(Cmd_AStore arg) = 0x3a
serialize(Cmd_IStore0) = 0x3b
serialize(Cmd_IStore1) = 0x3c
serialize(Cmd_IStore2) = 0x3d
serialize(Cmd_IStore3) = 0x3e
serialize(Cmd_LStore0) = 0x3f
serialize(Cmd_LStore1) = 0x40
serialize(Cmd_LStore2) = 0x41
serialize(Cmd_LStore3) = 0x42
serialize(Cmd_FStore0) = 0x43
serialize(Cmd_FStore1) = 0x44
serialize(Cmd_FStore2) = 0x45
serialize(Cmd_FStore3) = 0x46
serialize(Cmd_DStore0) = 0x47
serialize(Cmd_DStore1) = 0x48
serialize(Cmd_DStore2) = 0x49
serialize(Cmd_DStore3) = 0x4a
serialize(Cmd_AStore0) = 0x4b
serialize(Cmd_AStore1) = 0x4c
serialize(Cmd_AStore2) = 0x4d
serialize(Cmd_AStore3) = 0x4e
serialize(Cmd_IAStore) = 0x4f
serialize(Cmd_LAStore) = 0x50
serialize(Cmd_FAStore) = 0x51
serialize(Cmd_DAStore) = 0x52
serialize(Cmd_AAStore) = 0x53
serialize(Cmd_BAStore) = 0x54
serialize(Cmd_CAStore) = 0x55
serialize(Cmd_SAStore) = 0x56
serialize(Cmd_Pop) = 0x57
serialize(Cmd_Pop2) = 0x58
serialize(Cmd_Dup) = 0x59
serialize(Cmd_DupX1) = 0x5a
serialize(Cmd_DupX2) = 0x5b
serialize(Cmd_Dup2) = 0x5c
serialize(Cmd_Dup2X1) = 0x5d
serialize(Cmd_Dup2X2) = 0x5e
serialize(Cmd_Swap) = 0x5f
serialize(Cmd_IAdd) = 0x60
serialize(Cmd_LAdd) = 0x61
serialize(Cmd_FAdd) = 0x62
serialize(Cmd_DAdd) = 0x63
serialize(Cmd_ISub) = 0x64
serialize(Cmd_LSub) = 0x65
serialize(Cmd_FSub) = 0x66
serialize(Cmd_DSub) = 0x67
serialize(Cmd_IMul) = 0x68
serialize(Cmd_LMul) = 0x69
serialize(Cmd_FMul) = 0x6a
serialize(Cmd_DMul) = 0x6b
serialize(Cmd_IDiv) = 0x6c
serialize(Cmd_LDiv) = 0x6d
serialize(Cmd_FDiv) = 0x6e
serialize(Cmd_DDiv) = 0x6f
serialize(Cmd_IRem) = 0x70
serialize(Cmd_LRem) = 0x71
serialize(Cmd_FRem) = 0x72
serialize(Cmd_DRem) = 0x73
serialize(Cmd_INeg) = 0x74
serialize(Cmd_LNeg) = 0x75
serialize(Cmd_FNeg) = 0x76
serialize(Cmd_DNeg) = 0x77
serialize(Cmd_IShL) = 0x78
serialize(Cmd_LShL) = 0x79
serialize(Cmd_IShR) = 0x7a
serialize(Cmd_LShR) = 0x7b
serialize(Cmd_IUShR) = 0x7c
serialize(Cmd_LUShR) = 0x7d
serialize(Cmd_IAnd) = 0x7e
serialize(Cmd_LAnd) = 0x7f
serialize(Cmd_IOr) = 0x80
serialize(Cmd_LOr) = 0x81
serialize(Cmd_IXor) = 0x82
serialize(Cmd_LXor) = 0x83
serialize(Cmd_IInc arg1 arg2) = 0x84
serialize(Cmd_I2L) = 0x85
serialize(Cmd_I2F) = 0x86
serialize(Cmd_I2D) = 0x87
serialize(Cmd_L2I) = 0x88
serialize(Cmd_L2F) = 0x89
serialize(Cmd_L2D) = 0x8a
serialize(Cmd_F2I) = 0x8b
serialize(Cmd_F2L) = 0x8c
serialize(Cmd_F2D) = 0x8d
serialize(Cmd_D2I) = 0x8e
serialize(Cmd_D2L) = 0x8f
serialize(Cmd_D2F) = 0x90
serialize(Cmd_I2B) = 0x91
serialize(Cmd_I2C) = 0x92
serialize(Cmd_I2S) = 0x93
serialize(Cmd_LCmp) = 0x94
serialize(Cmd_FCmpL) = 0x95
serialize(Cmd_FCmpG) = 0x96
serialize(Cmd_DCmpL) = 0x97
serialize(Cmd_DCmpG) = 0x98
serialize(Cmd_IfEq arg1 arg2) = 0x99
serialize(Cmd_IfNe arg1 arg2) = 0x9a
serialize(Cmd_IfLt arg1 arg2) = 0x9b
serialize(Cmd_IfGe arg1 arg2) = 0x9c
serialize(Cmd_IfGt arg1 arg2) = 0x9d
serialize(Cmd_IfLe arg1 arg2) = 0x9e
serialize(Cmd_IfICmpEq arg1 arg2) = 0x9f
serialize(Cmd_IfICmpNe arg1 arg2) = 0xa0
serialize(Cmd_IfICmpLt arg1 arg2) = 0xa1
serialize(Cmd_IfICmpGe arg1 arg2) = 0xa2
serialize(Cmd_IfICmpGt arg1 arg2) = 0xa3
serialize(Cmd_IfICmpLe arg1 arg2) = 0xa4
serialize(Cmd_IfACmpEq arg1 arg2) = 0xa5
serialize(Cmd_IfACmpNe arg1 arg2) = 0xa6
serialize(Cmd_Goto arg1 arg2) = 0xa7
serialize(Cmd_JSr arg1 arg2) = 0xa8
serialize(Cmd_Ret arg) = 0xa9
serialize(Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args) = 0xaa
serialize(Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args) = 0xab
serialize(Cmd_IReturn) = 0xac
serialize(Cmd_LReturn) = 0xad
serialize(Cmd_FReturn) = 0xae
serialize(Cmd_DReturn) = 0xaf
serialize(Cmd_AReturn) = 0xb0
serialize(Cmd_Return) = 0xb1
serialize(Cmd_GetStatic arg1 arg2) = 0xb2
serialize(Cmd_PutStatic arg1 arg2) = 0xb3
serialize(Cmd_GetField arg1 arg2) = 0xb4
serialize(Cmd_PutField arg1 arg2) = 0xb5
serialize(Cmd_InvokeV arg1 arg2) = 0xb6
serialize(Cmd_InvokeSp arg1 arg2) = 0xb7
serialize(Cmd_InvokeSt arg1 arg2) = 0xb8
serialize(Cmd_InvokeIF arg1 arg2) = 0xb9
serialize(Cmd_InvokeD arg1 arg2) = 0xba
serialize(Cmd_New arg1 arg2) = 0xbb
serialize(Cmd_NewArray arg) = 0xbc
serialize(Cmd_ANewArray arg1 arg2) = 0xbd
serialize(Cmd_ArrayLenght) = 0xbe
serialize(Cmd_AThrow) = 0xbf
serialize(Cmd_CheckCast arg1 arg2) = 0xc0
serialize(Cmd_InstanceOf arg1 arg2) = 0xc1
serialize(Cmd_MonitorEnter) = 0xc2
serialize(Cmd_MonitorExit) = 0xc3
serialize(Cmd_Wide_ILoad arg1 arg2) = 0xc4
serialize(Cmd_Wide_LLoad arg1 arg2) = 0xc4
serialize(Cmd_Wide_FLoad arg1 arg2) = 0xc4
serialize(Cmd_Wide_DLoad arg1 arg2) = 0xc4
serialize(Cmd_Wide_ALoad arg1 arg2) = 0xc4
serialize(Cmd_Wide_IStore arg1 arg2) = 0xc4
serialize(Cmd_Wide_LStore arg1 arg2) = 0xc4
serialize(Cmd_Wide_FStore arg1 arg2) = 0xc4
serialize(Cmd_Wide_DStore arg1 arg2) = 0xc4
serialize(Cmd_Wide_AStore arg1 arg2) = 0xc4
serialize(Cmd_Wide_Ret arg1 arg2) = 0xc4
serialize(Cmd_Wide_IInc arg1 arg2 arg3 arg4) = 0xc4
serialize(Cmd_MultiANewArray arg1 arg2 arg3) = 0xc5
serialize(Cmd_IfNull arg1 arg2) = 0xc6
serialize(Cmd_IfNonNull arg1 arg2) = 0xc7
serialize(Cmd_GotoW arg1 arg2 arg3 arg4) = 0xc8
serialize(Cmd_JSrW arg1 arg2 arg3 arg4) = 0xc9
serialize(Breakpoint) = 0xca
serialize(ImpDep1) = 0xfe
serialize(ImpDep2) = 0xff



serializeAll :: [Command] -> [Word8]
serializeAll(x) = map s x where s a = serialize(a)



-- calculate Byte-Length of compiled Command , return (Offset + Length)

getNextOffset :: (Command, Int) -> Int

getNextOffset(Cmd_BIPush arg, offset) = (offset + 2)
getNextOffset(Cmd_SIPush arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_LdC_I arg, offset) = (offset + 2)
getNextOffset(Cmd_LdC_F arg, offset) = (offset + 2)
getNextOffset(Cmd_LdC_S arg, offset) = (offset + 2)
getNextOffset(Cmd_LdCW_I arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_LdCW_F arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_LdCW_S arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_LdC2W_L arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_LdC2W_D arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_ILoad arg, offset) = (offset + 2)
getNextOffset(Cmd_LLoad arg, offset) = (offset + 2)
getNextOffset(Cmd_FLoad arg, offset) = (offset + 2)
getNextOffset(Cmd_DLoad arg, offset) = (offset + 2)
getNextOffset(Cmd_ALoad arg, offset) = (offset + 2)
getNextOffset(Cmd_IStore arg, offset) = (offset + 2)
getNextOffset(Cmd_LStore arg, offset) = (offset + 2)
getNextOffset(Cmd_FStore arg, offset) = (offset + 2)
getNextOffset(Cmd_DStore arg, offset) = (offset + 2)
getNextOffset(Cmd_AStore arg, offset) = (offset + 2)
getNextOffset(Cmd_IInc arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfEq arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfNe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfLt arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfGe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfGt arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfLe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfICmpEq arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfICmpNe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfICmpLt arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfICmpGe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfICmpGt arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfICmpLe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfACmpEq arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfACmpNe arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_Goto arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_JSr arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_Ret arg, offset) = (offset + 2)
getNextOffset(Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args, offset) = (offset  + (3 - (rem offset 4)) + 13 + (((useValueOf(toInt [arg9, argA, argB, argC])) - (useValueOf(toInt [arg5, arg6, arg7, arg8])) + 1) * 4))
getNextOffset(Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args, offset) = (offset + (3 - (rem offset 4)) + 9 + ((useValueOf(toInt [arg5, arg6, arg7, arg8])) * 8))
getNextOffset(Cmd_GetStatic arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_PutStatic arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_GetField arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_PutField arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_InvokeV arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_InvokeSp arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_InvokeSt arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_InvokeIF arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_InvokeD arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_New arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_NewArray arg, offset) = (offset + 2)
getNextOffset(Cmd_ANewArray arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_CheckCast arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_InstanceOf arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_Wide_ILoad arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_LLoad arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_FLoad arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_DLoad arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_ALoad arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_IStore arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_LStore arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_FStore arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_DStore arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_AStore arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_Ret arg1 arg2, offset) = (offset + 4)
getNextOffset(Cmd_Wide_IInc arg1 arg2 arg3 arg4, offset) = (offset + 6)
getNextOffset(Cmd_MultiANewArray arg1 arg2 arg3, offset) = (offset + 4)
getNextOffset(Cmd_IfNull arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_IfNonNull arg1 arg2, offset) = (offset + 3)
getNextOffset(Cmd_GotoW arg1 arg2 arg3 arg4, offset) = (offset + 5)
getNextOffset(Cmd_JSrW arg1 arg2 arg3 arg4, offset) = (offset + 5)
getNextOffset(x, offset) = (offset + 1)



-- assign Offsets to Commands with Reference-Label (Indexed) and return prospected Byte-Size of compilated File

calculateFileOffsets :: Int -> [Indexed Command] -> ([Indexed (Command, Int)], Int)
calculateFileOffsets offset [] = ([], offset)
calculateFileOffsets offset ((i, x) : xs) = (((i, (x, offset)) : (fst (calculateFileOffsets (getNextOffset(x, offset)) xs))), snd (calculateFileOffsets (getNextOffset(x, offset)) xs))




-- assign Offsets to Commands with Reference-Label

assignOffsets :: Int -> [Indexed Command] -> [(Command, Int)]
assignOffsets i x = map snd (fst (calculateFileOffsets i x))



-- calculate prospected Byte-Size of compiled File

calculateFileSize :: Int -> [Indexed Command] -> Int
calculateFileSize i x = snd (calculateFileOffsets i x)



-- construct an IndexLookup for actual In-File-Offsets for each Index-Label

offsetLookup :: Int -> [Indexed Command] -> IndexLookup Int
offsetLookup offset lst = listInsert NewIndexLookup (map f (fst (calculateFileOffsets offset lst))) where f (i, (a, b)) = (i, b)



-- update Reference-Args to target value

unpackInt :: Maybe Int -> Int

unpackInt Nothing = 0
unpackInt (Just x) = x

setRefArgsList :: IndexLookup Int -> Args -> Args

setRefArgsList il (a : b : c : d : xs) = fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [a, b, c, d]))))
setRefArgsList il [] = []
setRefArgsList il (a : []) = []
setRefArgsList il (a : b : []) = []
setRefArgsList il (a : b : c : []) = []

setRefArgsListSkip :: IndexLookup Int -> Args -> Args

setRefArgsListSkip il (a : b : c : d : e : f : g : h : xs) = a : b : c : d : (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [e, f, g, h])))))
setRefArgsListSkip il [] = []
setRefArgsListSkip il (a : []) = []
setRefArgsListSkip il (a : b : []) = []
setRefArgsListSkip il (a : b : c : []) = []
setRefArgsListSkip il (a : b : c : d : []) = []
setRefArgsListSkip il (a : b : c : d : e : []) = []
setRefArgsListSkip il (a : b : c : d : e : f : []) = []
setRefArgsListSkip il (a : b : c : d : e : f : g : []) = []

setRefArgs :: IndexLookup Int -> Command -> Command

setRefArgs il (Cmd_IfEq arg1 arg2) = Cmd_IfEq (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfNe arg1 arg2) = Cmd_IfNe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfLt arg1 arg2) = Cmd_IfLt (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfGe arg1 arg2) = Cmd_IfGe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfGt arg1 arg2) = Cmd_IfGt (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfLe arg1 arg2) = Cmd_IfLe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfICmpEq arg1 arg2) = Cmd_IfICmpEq (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfICmpNe arg1 arg2) = Cmd_IfICmpNe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfICmpLt arg1 arg2) = Cmd_IfICmpLt (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfICmpGe arg1 arg2) = Cmd_IfICmpGe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfICmpGt arg1 arg2) = Cmd_IfICmpGt (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfICmpLe arg1 arg2) = Cmd_IfICmpLe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfACmpEq arg1 arg2) = Cmd_IfACmpEq (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_IfACmpNe arg1 arg2) = Cmd_IfACmpNe (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_Goto arg1 arg2) = Cmd_Goto (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_JSr arg1 arg2) = Cmd_JSr (head (fromShort (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2])))))) (last (fromShort (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2]))))))
setRefArgs il (Cmd_TableSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 argA argB argC args) = Cmd_TableSwitch padding (head (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4])))))) (head (tail (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4]))))))) (head (tail (tail (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4])))))))) (last (fromInt (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4])))))) arg5 arg6 arg7 arg8 arg9 argA argB argC (setRefArgsList il args)
setRefArgs il (Cmd_LookupSwitch padding arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 args) = Cmd_LookupSwitch padding (head (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4])))))) (head (tail (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4]))))))) (head (tail (tail (fromInt (useValueOf (unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4])))))))) (last (fromInt (useValueOf(unpackInt (il ?# useValueOf(toInt [arg1, arg2, arg3, arg4])))))) arg5 arg6 arg7 arg8 (setRefArgsListSkip il args)
setRefArgs il x = x

--TableSwitch
--LookupSwitch


-- update References from Index-Labels to actual In-File-Offsets

updateRefs :: IndexLookup Int -> [(Command, Int)] -> [(Command, Int)]
updateRefs il [] = []
updateRefs il (x : xs) = ((setRefArgs il (fst x)) , (snd x)) : (updateRefs il xs)


-- Constant Tag Representation

data ConstTag = CTg_Utf8         -- 1
              | CTg_Int          -- 3
              | CTg_Float        -- 4
              | CTg_Long         -- 5
              | CTg_Double       -- 6
              | CTg_Class        -- 7
              | CTg_String       -- 8
              | CTg_Field        -- 9
              | CTg_MethRef      -- 10
              | CTg_IFRef        -- 11
              | CTg_Name         -- 12
              | CTg_MethHandle   -- 15
              | CTg_MethType     -- 16
              | CTg_InvDyn       -- 18

translateConstTag :: ConstTag -> Word8

translateConstTag CTg_Utf8 = 1
translateConstTag CTg_Int = 3
translateConstTag CTg_Float = 4
translateConstTag CTg_Long = 5
translateConstTag CTg_Double = 6
translateConstTag CTg_Class = 7
translateConstTag CTg_String = 8
translateConstTag CTg_Field = 9
translateConstTag CTg_MethRef = 10
translateConstTag CTg_IFRef = 11
translateConstTag CTg_Name = 12
translateConstTag CTg_MethHandle = 15
translateConstTag CTg_MethType = 16
translateConstTag CTg_InvDyn = 18


-- add Constant to ConstPool

addConst :: (Word8, Word8) -> ConstTag -> [[Word8]] -> (IndexLookup Int) -> (Int, [[Word8]], (IndexLookup Int))

addConst (a, b) tag consts il | ((il ?# useValueOf(toInt [a, b])) == Nothing) = ((snd (insertVal consts (a, b) tag 0)), (fst (insertVal consts (a, b) tag 0)), (il !# useValueOf(toInt [a, b]) # (snd (insertVal consts (a, b) tag 0))))
                              | otherwise = ((unpackInt (il ?# useValueOf(toInt [a, b]))), consts, il) where insertVal (x : xs) (a, b) tag n = ((x : (fst (insertVal xs (a, b) tag (n+1)))), (snd (insertVal xs (a, b) tag (n+1))))
                                                                                                             insertVal [] (a, b) tag n = (([(translateConstTag tag) , a , b] : []), n)


addConst32 :: (Word8, Word8, Word8, Word8) -> ConstTag -> [[Word8]] -> (IndexLookup Int) -> (Int, [[Word8]], (IndexLookup Int))

addConst32 (a, b, c, d) tag consts il | ((il ?# useValueOf(toInt [a, b, c, d])) == Nothing) = ((snd (insertVal consts (a, b, c, d) tag 0)), (fst (insertVal consts (a, b, c, d) tag 0)), (il !# useValueOf(toInt [a, b, c, d]) # (snd (insertVal consts (a, b, c, d) tag 0))))
                                      | otherwise = ((unpackInt (il ?# useValueOf(toInt [a, b, c, d]))), consts, il) where insertVal (x : xs) (a, b, c, d) tag n = ((x : (fst (insertVal xs (a, b, c, d) tag (n+1)))), (snd (insertVal xs (a, b, c, d) tag (n+1))))
                                                                                                                           insertVal [] (a, b, c, d) tag n = (([(translateConstTag tag) , a , b , c , d] : []), n)


addConstStr :: String -> ConstTag -> [[Word8]] -> (TagLookup Int) -> (Int, [[Word8]], (TagLookup Int))

addConstStr a tag consts tl | ((tl ?% a) == Nothing) = ((snd (insertStr consts a tag 0)), (fst (insertStr consts a tag 0)), (tl !% a % (snd (insertStr consts a tag 0))))
                            | otherwise = ((unpackInt (tl ?% a)), consts, tl) where insertStr (x : xs) a tag n = ((x : (fst (insertStr xs a tag (n+1)))), (snd (insertStr xs a tag (n+1)))) 
                                                                                    insertStr [] a tag n = ([(translateConstTag tag) : (fromString a)], n)


-- shift ConstantPool Index for long Constants by value (length of const16)

updateConstI :: Command -> Int -> Command
updateConstI (Cmd_LdC_I arg) i = Cmd_LdC_I (last (fromShort (useValueOf (useValueOf(toInt [arg]) + i))))
updateConstI (Cmd_LdCW_I arg1 arg2) i = Cmd_LdCW_I (head (fromShort (useValueOf (useValueOf(toInt [arg1, arg2]) + i)))) (last (fromShort (useValueOf(useValueOf(toInt [arg1, arg2]) + i))))
updateConstI x i = x

updateConstF :: Command -> Int -> Command
updateConstF (Cmd_LdC_F arg) i = Cmd_LdC_F (last (fromShort (useValueOf (useValueOf(toInt [arg]) + i))))
updateConstF (Cmd_LdCW_F arg1 arg2) i = Cmd_LdCW_F (head (fromShort (useValueOf (useValueOf(toInt [arg1, arg2]) + i)))) (last (fromShort (useValueOf(useValueOf(toInt [arg1, arg2]) + i))))
updateConstF x i = x

updateConstS :: Command -> Int -> Command
updateConstS (Cmd_LdC_S arg) i = Cmd_LdC_S (last (fromShort (useValueOf (useValueOf(toInt [arg]) + i))))
updateConstS (Cmd_LdCW_S arg1 arg2) i = Cmd_LdCW_S (head (fromShort (useValueOf (useValueOf(toInt [arg1, arg2]) + i)))) (last (fromShort (useValueOf(useValueOf(toInt [arg1, arg2]) + i))))
updateConstS x i = x

updateConstL :: Command -> Int -> Command
updateConstL (Cmd_LdC2W_L arg1 arg2) i = Cmd_LdC2W_L (head (fromShort (useValueOf (useValueOf(toInt [arg1, arg2]) + i)))) (last (fromShort (useValueOf(useValueOf(toInt [arg1, arg2]) + i))))
updateConstL x i = x

updateConstD :: Command -> Int -> Command
updateConstD (Cmd_LdC2W_D arg1 arg2) i = Cmd_LdC2W_D (head (fromShort (useValueOf (useValueOf(toInt [arg1, arg2]) + i)))) (last (fromShort (useValueOf(useValueOf(toInt [arg1, arg2]) + i))))
updateConstD x i = x



-- finalize ConstantPool by "unpacking" and concatenating partial ConstPools

finalizeConstPool :: [[[Word8]]] -> [Word8]
finalizeConstPool x = foldl (++) [] (map unpack x) where unpack x = foldl (++) [] x




--- Access Flags

data AccessFlag = Acc_Public            -- 0x0001
                | Acc_Final             -- 0x0010
                | Acc_Super             -- 0x0020
                | Acc_Interface         -- 0x0200
                | Acc_Abstract          -- 0x0400
                | Acc_Synthetic         -- 0x1000
                | Acc_Annotation        -- 0x2000
                | Acc_Enum              -- 0x4000


setAccessFlags :: [AccessFlag] -> [Word8]

setAccessFlags lst = fromShort(foldr (+) 0 (map translate lst)) where translate Acc_Public = 0x0001
                                                                      translate Acc_Final = 0x0010
                                                                      translate Acc_Super = 0x0020
                                                                      translate Acc_Interface = 0x0200
                                                                      translate Acc_Abstract = 0x0400
                                                                      translate Acc_Synthetic = 0x1000
                                                                      translate Acc_Annotation = 0x2000
                                                                      translate Acc_Enum = 0x4000



-- access-Operations for 3-Tuples

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c




-- Benjamin Hellstern, Magdalena Sannwald
-- Seminar Compilerbau WS 14/15
-- WSI Informatik, Ernst-Bloch-Universität Tübingen