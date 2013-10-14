module Language.Pig.Parser.AST (
         PigNode(..)
       , PigOperator(..)
       ) where

-- source:
-- http://wiki.apache.org/pig/PigLexer
-- http://wiki.apache.org/pig/PigParser

data PigNode = PigSeq [PigNode]
             | PigAssignment PigNode PigNode
             | PigDescribe PigNode
             | PigIdentifier String
             | PigOpClause PigNode
             | PigLoadClause PigNode PigNode PigNode
             | PigForeachClause PigNode PigNode
             | PigInnerJoinClause [PigNode]
             | PigGroupClause PigNode PigNode
             | PigDefineUDF PigNode PigNode PigNode
             | PigStreamClause PigNode PigNode PigNode
             | PigStore PigNode PigNode PigNode
             | PigShip PigNode
             | PigFilename String
             | PigDirectory String
             | PigExec String
             | PigPath String
             | PigFunc String PigNode
             | PigArguments [PigNode]
             | PigSchema [PigNode]
             | PigField PigNode PigNode
             | PigFieldName String
             | PigFieldType PigNode
             | PigTransforms [PigNode]
             | PigFlatten String PigNode -- foreach flatten transform
             | PigTupleFieldGlob
             | PigTuple [PigNode]
             | PigExpressionTransform PigNode PigNode -- foreach calculates expression
             | PigExpression PigNode
             | PigJoin String String
             | PigUnary PigOperator PigNode
             | PigBinary PigOperator PigNode PigNode
             | PigBooleanUnary PigOperator PigNode
             | PigBooleanBinary PigOperator PigNode PigNode
             | PigBinCond PigNode PigNode PigNode
             | PigString String
             | PigInt
             | PigLong
             | PigFloat
             | PigDouble
             | PigCharArray
             | PigByteArray
             | PigNumber (Either Integer Double)
             deriving (Show, Eq) -- Read, Data, Typeable ?

data PigOperator = PigNeg
                 | PigAdd
                 | PigSubtract
                 | PigMultiply
                 | PigDivide
                 | PigModulo
                 | PigAnd
                 | PigOr
                 | PigNot
                 | PigEqual
                 | PigNotEqual
                 | PigGreater
                 | PigLess
                 | PigGreaterEqual
                 | PigLessEqual
                 deriving (Show, Eq)

