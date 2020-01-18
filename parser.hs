{-# OPTIONS_GHC -w #-}
module Parser where
import qualified Lexer as L
import qualified AST as S
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (L.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

action_0 (30) = happyShift action_7
action_0 (31) = happyShift action_8
action_0 (32) = happyShift action_9
action_0 (34) = happyShift action_10
action_0 (35) = happyShift action_11
action_0 (36) = happyShift action_12
action_0 (37) = happyShift action_13
action_0 (38) = happyShift action_14
action_0 (39) = happyShift action_15
action_0 (40) = happyShift action_16
action_0 (41) = happyShift action_17
action_0 (45) = happyShift action_18
action_0 (4) = happyGoto action_19
action_0 (5) = happyGoto action_20
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 (12) = happyGoto action_6
action_0 _ = happyFail

action_1 (30) = happyShift action_7
action_1 (31) = happyShift action_8
action_1 (32) = happyShift action_9
action_1 (34) = happyShift action_10
action_1 (35) = happyShift action_11
action_1 (36) = happyShift action_12
action_1 (37) = happyShift action_13
action_1 (38) = happyShift action_14
action_1 (39) = happyShift action_15
action_1 (40) = happyShift action_16
action_1 (41) = happyShift action_17
action_1 (45) = happyShift action_18
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 (12) = happyGoto action_6
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (48) = happyShift action_45
action_3 _ = happyFail

action_4 _ = happyReduce_5

action_5 (15) = happyShift action_32
action_5 (16) = happyShift action_33
action_5 (17) = happyShift action_34
action_5 (18) = happyShift action_35
action_5 (19) = happyShift action_36
action_5 (20) = happyShift action_37
action_5 (21) = happyShift action_38
action_5 (22) = happyShift action_39
action_5 (23) = happyShift action_40
action_5 (24) = happyShift action_41
action_5 (27) = happyShift action_42
action_5 (41) = happyShift action_43
action_5 (45) = happyShift action_44
action_5 _ = happyReduce_9

action_6 _ = happyReduce_24

action_7 (41) = happyShift action_31
action_7 _ = happyFail

action_8 (30) = happyShift action_7
action_8 (34) = happyShift action_10
action_8 (35) = happyShift action_11
action_8 (38) = happyShift action_14
action_8 (39) = happyShift action_24
action_8 (40) = happyShift action_16
action_8 (41) = happyShift action_17
action_8 (45) = happyShift action_18
action_8 (11) = happyGoto action_30
action_8 (12) = happyGoto action_6
action_8 _ = happyFail

action_9 (39) = happyShift action_29
action_9 _ = happyFail

action_10 _ = happyReduce_19

action_11 _ = happyReduce_20

action_12 (30) = happyShift action_7
action_12 (34) = happyShift action_10
action_12 (35) = happyShift action_11
action_12 (38) = happyShift action_14
action_12 (39) = happyShift action_24
action_12 (40) = happyShift action_16
action_12 (41) = happyShift action_17
action_12 (45) = happyShift action_18
action_12 (11) = happyGoto action_28
action_12 (12) = happyGoto action_6
action_12 _ = happyFail

action_13 (30) = happyShift action_7
action_13 (34) = happyShift action_10
action_13 (35) = happyShift action_11
action_13 (38) = happyShift action_14
action_13 (39) = happyShift action_24
action_13 (40) = happyShift action_16
action_13 (41) = happyShift action_17
action_13 (45) = happyShift action_18
action_13 (11) = happyGoto action_27
action_13 (12) = happyGoto action_6
action_13 _ = happyFail

action_14 _ = happyReduce_18

action_15 (29) = happyShift action_26
action_15 _ = happyReduce_23

action_16 _ = happyReduce_22

action_17 (30) = happyShift action_7
action_17 (34) = happyShift action_10
action_17 (35) = happyShift action_11
action_17 (38) = happyShift action_14
action_17 (39) = happyShift action_24
action_17 (40) = happyShift action_16
action_17 (41) = happyShift action_17
action_17 (45) = happyShift action_18
action_17 (11) = happyGoto action_25
action_17 (12) = happyGoto action_6
action_17 _ = happyFail

action_18 (30) = happyShift action_7
action_18 (34) = happyShift action_10
action_18 (35) = happyShift action_11
action_18 (38) = happyShift action_14
action_18 (39) = happyShift action_24
action_18 (40) = happyShift action_16
action_18 (41) = happyShift action_17
action_18 (45) = happyShift action_18
action_18 (11) = happyGoto action_22
action_18 (12) = happyGoto action_6
action_18 (13) = happyGoto action_23
action_18 _ = happyReduce_40

action_19 (49) = happyAccept
action_19 _ = happyFail

action_20 (30) = happyShift action_7
action_20 (31) = happyShift action_8
action_20 (32) = happyShift action_9
action_20 (34) = happyShift action_10
action_20 (35) = happyShift action_11
action_20 (36) = happyShift action_12
action_20 (37) = happyShift action_13
action_20 (38) = happyShift action_14
action_20 (39) = happyShift action_15
action_20 (40) = happyShift action_16
action_20 (41) = happyShift action_17
action_20 (45) = happyShift action_18
action_20 (4) = happyGoto action_21
action_20 (5) = happyGoto action_20
action_20 (6) = happyGoto action_3
action_20 (7) = happyGoto action_4
action_20 (11) = happyGoto action_5
action_20 (12) = happyGoto action_6
action_20 _ = happyReduce_1

action_21 _ = happyReduce_2

action_22 (15) = happyShift action_32
action_22 (16) = happyShift action_33
action_22 (17) = happyShift action_34
action_22 (18) = happyShift action_35
action_22 (19) = happyShift action_36
action_22 (20) = happyShift action_37
action_22 (21) = happyShift action_38
action_22 (22) = happyShift action_39
action_22 (23) = happyShift action_40
action_22 (24) = happyShift action_41
action_22 (27) = happyShift action_42
action_22 (41) = happyShift action_43
action_22 (45) = happyShift action_44
action_22 (47) = happyShift action_68
action_22 _ = happyReduce_41

action_23 (46) = happyShift action_67
action_23 _ = happyFail

action_24 _ = happyReduce_23

action_25 (15) = happyShift action_32
action_25 (16) = happyShift action_33
action_25 (17) = happyShift action_34
action_25 (18) = happyShift action_35
action_25 (19) = happyShift action_36
action_25 (20) = happyShift action_37
action_25 (21) = happyShift action_38
action_25 (22) = happyShift action_39
action_25 (23) = happyShift action_40
action_25 (24) = happyShift action_41
action_25 (27) = happyShift action_42
action_25 (41) = happyShift action_43
action_25 (42) = happyShift action_66
action_25 (45) = happyShift action_44
action_25 _ = happyFail

action_26 (30) = happyShift action_7
action_26 (34) = happyShift action_10
action_26 (35) = happyShift action_11
action_26 (38) = happyShift action_14
action_26 (39) = happyShift action_24
action_26 (40) = happyShift action_16
action_26 (41) = happyShift action_17
action_26 (45) = happyShift action_18
action_26 (11) = happyGoto action_65
action_26 (12) = happyGoto action_6
action_26 _ = happyFail

action_27 (15) = happyShift action_32
action_27 (16) = happyShift action_33
action_27 (17) = happyShift action_34
action_27 (18) = happyShift action_35
action_27 (19) = happyShift action_36
action_27 (20) = happyShift action_37
action_27 (21) = happyShift action_38
action_27 (22) = happyShift action_39
action_27 (23) = happyShift action_40
action_27 (24) = happyShift action_41
action_27 (27) = happyShift action_42
action_27 (41) = happyShift action_43
action_27 (45) = happyShift action_44
action_27 _ = happyReduce_8

action_28 (15) = happyShift action_32
action_28 (16) = happyShift action_33
action_28 (17) = happyShift action_34
action_28 (18) = happyShift action_35
action_28 (19) = happyShift action_36
action_28 (20) = happyShift action_37
action_28 (21) = happyShift action_38
action_28 (22) = happyShift action_39
action_28 (23) = happyShift action_40
action_28 (24) = happyShift action_41
action_28 (27) = happyShift action_42
action_28 (41) = happyShift action_43
action_28 (43) = happyShift action_62
action_28 (45) = happyShift action_44
action_28 (9) = happyGoto action_64
action_28 _ = happyFail

action_29 (29) = happyShift action_63
action_29 _ = happyFail

action_30 (15) = happyShift action_32
action_30 (16) = happyShift action_33
action_30 (17) = happyShift action_34
action_30 (18) = happyShift action_35
action_30 (19) = happyShift action_36
action_30 (20) = happyShift action_37
action_30 (21) = happyShift action_38
action_30 (22) = happyShift action_39
action_30 (23) = happyShift action_40
action_30 (24) = happyShift action_41
action_30 (27) = happyShift action_42
action_30 (41) = happyShift action_43
action_30 (43) = happyShift action_62
action_30 (45) = happyShift action_44
action_30 (9) = happyGoto action_61
action_30 _ = happyFail

action_31 (39) = happyShift action_60
action_31 (14) = happyGoto action_59
action_31 _ = happyReduce_43

action_32 (30) = happyShift action_7
action_32 (34) = happyShift action_10
action_32 (35) = happyShift action_11
action_32 (38) = happyShift action_14
action_32 (39) = happyShift action_24
action_32 (40) = happyShift action_16
action_32 (41) = happyShift action_17
action_32 (45) = happyShift action_18
action_32 (11) = happyGoto action_58
action_32 (12) = happyGoto action_6
action_32 _ = happyFail

action_33 (30) = happyShift action_7
action_33 (34) = happyShift action_10
action_33 (35) = happyShift action_11
action_33 (38) = happyShift action_14
action_33 (39) = happyShift action_24
action_33 (40) = happyShift action_16
action_33 (41) = happyShift action_17
action_33 (45) = happyShift action_18
action_33 (11) = happyGoto action_57
action_33 (12) = happyGoto action_6
action_33 _ = happyFail

action_34 (30) = happyShift action_7
action_34 (34) = happyShift action_10
action_34 (35) = happyShift action_11
action_34 (38) = happyShift action_14
action_34 (39) = happyShift action_24
action_34 (40) = happyShift action_16
action_34 (41) = happyShift action_17
action_34 (45) = happyShift action_18
action_34 (11) = happyGoto action_56
action_34 (12) = happyGoto action_6
action_34 _ = happyFail

action_35 (30) = happyShift action_7
action_35 (34) = happyShift action_10
action_35 (35) = happyShift action_11
action_35 (38) = happyShift action_14
action_35 (39) = happyShift action_24
action_35 (40) = happyShift action_16
action_35 (41) = happyShift action_17
action_35 (45) = happyShift action_18
action_35 (11) = happyGoto action_55
action_35 (12) = happyGoto action_6
action_35 _ = happyFail

action_36 (30) = happyShift action_7
action_36 (34) = happyShift action_10
action_36 (35) = happyShift action_11
action_36 (38) = happyShift action_14
action_36 (39) = happyShift action_24
action_36 (40) = happyShift action_16
action_36 (41) = happyShift action_17
action_36 (45) = happyShift action_18
action_36 (11) = happyGoto action_54
action_36 (12) = happyGoto action_6
action_36 _ = happyFail

action_37 (30) = happyShift action_7
action_37 (34) = happyShift action_10
action_37 (35) = happyShift action_11
action_37 (38) = happyShift action_14
action_37 (39) = happyShift action_24
action_37 (40) = happyShift action_16
action_37 (41) = happyShift action_17
action_37 (45) = happyShift action_18
action_37 (11) = happyGoto action_53
action_37 (12) = happyGoto action_6
action_37 _ = happyFail

action_38 (30) = happyShift action_7
action_38 (34) = happyShift action_10
action_38 (35) = happyShift action_11
action_38 (38) = happyShift action_14
action_38 (39) = happyShift action_24
action_38 (40) = happyShift action_16
action_38 (41) = happyShift action_17
action_38 (45) = happyShift action_18
action_38 (11) = happyGoto action_52
action_38 (12) = happyGoto action_6
action_38 _ = happyFail

action_39 (30) = happyShift action_7
action_39 (34) = happyShift action_10
action_39 (35) = happyShift action_11
action_39 (38) = happyShift action_14
action_39 (39) = happyShift action_24
action_39 (40) = happyShift action_16
action_39 (41) = happyShift action_17
action_39 (45) = happyShift action_18
action_39 (11) = happyGoto action_51
action_39 (12) = happyGoto action_6
action_39 _ = happyFail

action_40 (30) = happyShift action_7
action_40 (34) = happyShift action_10
action_40 (35) = happyShift action_11
action_40 (38) = happyShift action_14
action_40 (39) = happyShift action_24
action_40 (40) = happyShift action_16
action_40 (41) = happyShift action_17
action_40 (45) = happyShift action_18
action_40 (11) = happyGoto action_50
action_40 (12) = happyGoto action_6
action_40 _ = happyFail

action_41 (30) = happyShift action_7
action_41 (34) = happyShift action_10
action_41 (35) = happyShift action_11
action_41 (38) = happyShift action_14
action_41 (39) = happyShift action_24
action_41 (40) = happyShift action_16
action_41 (41) = happyShift action_17
action_41 (45) = happyShift action_18
action_41 (11) = happyGoto action_49
action_41 (12) = happyGoto action_6
action_41 _ = happyFail

action_42 (30) = happyShift action_7
action_42 (34) = happyShift action_10
action_42 (35) = happyShift action_11
action_42 (38) = happyShift action_14
action_42 (39) = happyShift action_24
action_42 (40) = happyShift action_16
action_42 (41) = happyShift action_17
action_42 (45) = happyShift action_18
action_42 (11) = happyGoto action_48
action_42 (12) = happyGoto action_6
action_42 _ = happyFail

action_43 (30) = happyShift action_7
action_43 (34) = happyShift action_10
action_43 (35) = happyShift action_11
action_43 (38) = happyShift action_14
action_43 (39) = happyShift action_24
action_43 (40) = happyShift action_16
action_43 (41) = happyShift action_17
action_43 (45) = happyShift action_18
action_43 (11) = happyGoto action_22
action_43 (12) = happyGoto action_6
action_43 (13) = happyGoto action_47
action_43 _ = happyReduce_40

action_44 (30) = happyShift action_7
action_44 (34) = happyShift action_10
action_44 (35) = happyShift action_11
action_44 (38) = happyShift action_14
action_44 (39) = happyShift action_24
action_44 (40) = happyShift action_16
action_44 (41) = happyShift action_17
action_44 (45) = happyShift action_18
action_44 (11) = happyGoto action_46
action_44 (12) = happyGoto action_6
action_44 _ = happyFail

action_45 _ = happyReduce_3

action_46 (15) = happyShift action_32
action_46 (16) = happyShift action_33
action_46 (17) = happyShift action_34
action_46 (18) = happyShift action_35
action_46 (19) = happyShift action_36
action_46 (20) = happyShift action_37
action_46 (21) = happyShift action_38
action_46 (22) = happyShift action_39
action_46 (23) = happyShift action_40
action_46 (24) = happyShift action_41
action_46 (27) = happyShift action_42
action_46 (41) = happyShift action_43
action_46 (45) = happyShift action_44
action_46 (46) = happyShift action_79
action_46 _ = happyFail

action_47 (42) = happyShift action_78
action_47 _ = happyFail

action_48 (15) = happyShift action_32
action_48 (16) = happyShift action_33
action_48 (17) = happyShift action_34
action_48 (18) = happyShift action_35
action_48 (19) = happyShift action_36
action_48 (20) = happyShift action_37
action_48 (21) = happyShift action_38
action_48 (22) = happyShift action_39
action_48 (23) = happyShift action_40
action_48 (24) = happyShift action_41
action_48 (41) = happyShift action_43
action_48 (45) = happyShift action_44
action_48 _ = happyReduce_38

action_49 (15) = happyShift action_32
action_49 (16) = happyShift action_33
action_49 (17) = happyShift action_34
action_49 (18) = happyShift action_35
action_49 (19) = happyShift action_36
action_49 (20) = happyShift action_37
action_49 (21) = happyShift action_38
action_49 (22) = happyShift action_39
action_49 (23) = happyShift action_40
action_49 (41) = happyShift action_43
action_49 (45) = happyShift action_44
action_49 _ = happyReduce_37

action_50 (22) = happyFail
action_50 (23) = happyFail
action_50 (41) = happyShift action_43
action_50 (45) = happyShift action_44
action_50 _ = happyReduce_36

action_51 (22) = happyFail
action_51 (23) = happyFail
action_51 (41) = happyShift action_43
action_51 (45) = happyShift action_44
action_51 _ = happyReduce_35

action_52 (20) = happyFail
action_52 (21) = happyFail
action_52 (22) = happyShift action_39
action_52 (23) = happyShift action_40
action_52 (41) = happyShift action_43
action_52 (45) = happyShift action_44
action_52 _ = happyReduce_34

action_53 (20) = happyFail
action_53 (21) = happyFail
action_53 (22) = happyShift action_39
action_53 (23) = happyShift action_40
action_53 (41) = happyShift action_43
action_53 (45) = happyShift action_44
action_53 _ = happyReduce_33

action_54 (15) = happyShift action_32
action_54 (16) = happyShift action_33
action_54 (17) = happyShift action_34
action_54 (20) = happyShift action_37
action_54 (21) = happyShift action_38
action_54 (22) = happyShift action_39
action_54 (23) = happyShift action_40
action_54 (41) = happyShift action_43
action_54 (45) = happyShift action_44
action_54 _ = happyReduce_29

action_55 (15) = happyShift action_32
action_55 (16) = happyShift action_33
action_55 (17) = happyShift action_34
action_55 (20) = happyShift action_37
action_55 (21) = happyShift action_38
action_55 (22) = happyShift action_39
action_55 (23) = happyShift action_40
action_55 (41) = happyShift action_43
action_55 (45) = happyShift action_44
action_55 _ = happyReduce_28

action_56 (20) = happyShift action_37
action_56 (21) = happyShift action_38
action_56 (22) = happyShift action_39
action_56 (23) = happyShift action_40
action_56 (41) = happyShift action_43
action_56 (45) = happyShift action_44
action_56 _ = happyReduce_32

action_57 (20) = happyShift action_37
action_57 (21) = happyShift action_38
action_57 (22) = happyShift action_39
action_57 (23) = happyShift action_40
action_57 (41) = happyShift action_43
action_57 (45) = happyShift action_44
action_57 _ = happyReduce_31

action_58 (20) = happyShift action_37
action_58 (21) = happyShift action_38
action_58 (22) = happyShift action_39
action_58 (23) = happyShift action_40
action_58 (41) = happyShift action_43
action_58 (45) = happyShift action_44
action_58 _ = happyReduce_30

action_59 (42) = happyShift action_77
action_59 _ = happyFail

action_60 (47) = happyShift action_76
action_60 _ = happyReduce_44

action_61 (33) = happyShift action_75
action_61 (8) = happyGoto action_74
action_61 _ = happyReduce_11

action_62 (30) = happyShift action_7
action_62 (31) = happyShift action_8
action_62 (32) = happyShift action_9
action_62 (34) = happyShift action_10
action_62 (35) = happyShift action_11
action_62 (36) = happyShift action_12
action_62 (37) = happyShift action_13
action_62 (38) = happyShift action_14
action_62 (39) = happyShift action_15
action_62 (40) = happyShift action_16
action_62 (41) = happyShift action_17
action_62 (45) = happyShift action_18
action_62 (5) = happyGoto action_71
action_62 (6) = happyGoto action_3
action_62 (7) = happyGoto action_4
action_62 (10) = happyGoto action_72
action_62 (11) = happyGoto action_73
action_62 (12) = happyGoto action_6
action_62 _ = happyReduce_16

action_63 (30) = happyShift action_7
action_63 (34) = happyShift action_10
action_63 (35) = happyShift action_11
action_63 (38) = happyShift action_14
action_63 (39) = happyShift action_24
action_63 (40) = happyShift action_16
action_63 (41) = happyShift action_17
action_63 (45) = happyShift action_18
action_63 (11) = happyGoto action_70
action_63 (12) = happyGoto action_6
action_63 _ = happyFail

action_64 _ = happyReduce_4

action_65 (15) = happyShift action_32
action_65 (16) = happyShift action_33
action_65 (17) = happyShift action_34
action_65 (18) = happyShift action_35
action_65 (19) = happyShift action_36
action_65 (20) = happyShift action_37
action_65 (21) = happyShift action_38
action_65 (22) = happyShift action_39
action_65 (23) = happyShift action_40
action_65 (24) = happyShift action_41
action_65 (27) = happyShift action_42
action_65 (41) = happyShift action_43
action_65 (45) = happyShift action_44
action_65 _ = happyReduce_7

action_66 _ = happyReduce_25

action_67 _ = happyReduce_39

action_68 (30) = happyShift action_7
action_68 (34) = happyShift action_10
action_68 (35) = happyShift action_11
action_68 (38) = happyShift action_14
action_68 (39) = happyShift action_24
action_68 (40) = happyShift action_16
action_68 (41) = happyShift action_17
action_68 (45) = happyShift action_18
action_68 (11) = happyGoto action_22
action_68 (12) = happyGoto action_6
action_68 (13) = happyGoto action_69
action_68 _ = happyReduce_40

action_69 _ = happyReduce_42

action_70 (15) = happyShift action_32
action_70 (16) = happyShift action_33
action_70 (17) = happyShift action_34
action_70 (18) = happyShift action_35
action_70 (19) = happyShift action_36
action_70 (20) = happyShift action_37
action_70 (21) = happyShift action_38
action_70 (22) = happyShift action_39
action_70 (23) = happyShift action_40
action_70 (24) = happyShift action_41
action_70 (27) = happyShift action_42
action_70 (41) = happyShift action_43
action_70 (45) = happyShift action_44
action_70 _ = happyReduce_6

action_71 (30) = happyShift action_7
action_71 (31) = happyShift action_8
action_71 (32) = happyShift action_9
action_71 (34) = happyShift action_10
action_71 (35) = happyShift action_11
action_71 (36) = happyShift action_12
action_71 (37) = happyShift action_13
action_71 (38) = happyShift action_14
action_71 (39) = happyShift action_15
action_71 (40) = happyShift action_16
action_71 (41) = happyShift action_17
action_71 (45) = happyShift action_18
action_71 (5) = happyGoto action_71
action_71 (6) = happyGoto action_3
action_71 (7) = happyGoto action_4
action_71 (10) = happyGoto action_86
action_71 (11) = happyGoto action_5
action_71 (12) = happyGoto action_6
action_71 _ = happyReduce_16

action_72 (44) = happyShift action_85
action_72 _ = happyFail

action_73 (15) = happyShift action_32
action_73 (16) = happyShift action_33
action_73 (17) = happyShift action_34
action_73 (18) = happyShift action_35
action_73 (19) = happyShift action_36
action_73 (20) = happyShift action_37
action_73 (21) = happyShift action_38
action_73 (22) = happyShift action_39
action_73 (23) = happyShift action_40
action_73 (24) = happyShift action_41
action_73 (27) = happyShift action_42
action_73 (41) = happyShift action_43
action_73 (44) = happyShift action_84
action_73 (45) = happyShift action_44
action_73 _ = happyReduce_9

action_74 _ = happyReduce_10

action_75 (31) = happyShift action_8
action_75 (43) = happyShift action_62
action_75 (7) = happyGoto action_82
action_75 (9) = happyGoto action_83
action_75 _ = happyFail

action_76 (39) = happyShift action_60
action_76 (14) = happyGoto action_81
action_76 _ = happyReduce_43

action_77 (43) = happyShift action_62
action_77 (9) = happyGoto action_80
action_77 _ = happyFail

action_78 _ = happyReduce_27

action_79 _ = happyReduce_26

action_80 _ = happyReduce_21

action_81 _ = happyReduce_45

action_82 _ = happyReduce_13

action_83 _ = happyReduce_12

action_84 _ = happyReduce_15

action_85 _ = happyReduce_14

action_86 _ = happyReduce_17

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn11  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn5
		 (S.While (L.tokPosn happy_var_1) happy_var_2 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (let (L.Ident _ s) = happy_var_2 in S.Assign (L.tokPosn happy_var_1) s happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (let (L.Ident _ s) = happy_var_1 in S.Set (L.tokPosn happy_var_2) s happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (S.Return (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (S.ExprStmt happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (S.If (L.tokPosn happy_var_1) happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_0  8 happyReduction_11
happyReduction_11  =  HappyAbsSyn8
		 (Nothing
	)

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Just happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Just happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn10  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (S.Block (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn11  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (S.BlockExpr (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  10 happyReduction_16
happyReduction_16  =  HappyAbsSyn10
		 ([]
	)

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let (L.Int p i) = happy_var_1 in S.Int p i
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (S.Bool (L.tokPosn happy_var_1) True
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (S.Bool (L.tokPosn happy_var_1) False
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 5 11 happyReduction_21
happyReduction_21 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Func (L.tokPosn happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let (L.String p s) = happy_var_1 in S.String p s
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 ((\(L.Ident p s) -> S.Ident p s) happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  11 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 11 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Subscript (S.exprPosn happy_var_3) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 11 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Call  (S.exprPosn happy_var_1) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_3  11 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Plus happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  11 happyReduction_29
happyReduction_29 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Minus happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  11 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Times happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  11 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Divide happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  11 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Mod happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  11 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.LThan happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  11 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.GThan happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  11 happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.LTEq happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  11 happyReduction_36
happyReduction_36 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.GTEq happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  11 happyReduction_37
happyReduction_37 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.EqEq happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  11 happyReduction_38
happyReduction_38 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.OrOr happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  12 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (S.Array (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_0  13 happyReduction_40
happyReduction_40  =  HappyAbsSyn13
		 ([]
	)

happyReduce_41 = happySpecReduce_1  13 happyReduction_41
happyReduction_41 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  13 happyReduction_42
happyReduction_42 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_0  14 happyReduction_43
happyReduction_43  =  HappyAbsSyn14
		 ([]
	)

happyReduce_44 = happySpecReduce_1  14 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (let (L.Ident p s) = happy_var_1 in [s]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  14 happyReduction_45
happyReduction_45 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (let (L.Ident p s) = happy_var_1 in s : happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 49 49 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.ReservedOp _ "*" -> cont 15;
	L.ReservedOp _ "/" -> cont 16;
	L.ReservedOp _ "%" -> cont 17;
	L.ReservedOp _ "+" -> cont 18;
	L.ReservedOp _ "-" -> cont 19;
	L.ReservedOp _ "<" -> cont 20;
	L.ReservedOp _ ">" -> cont 21;
	L.ReservedOp _ "<=" -> cont 22;
	L.ReservedOp _ ">=" -> cont 23;
	L.ReservedOp _ "==" -> cont 24;
	L.ReservedOp _ "!=" -> cont 25;
	L.ReservedOp _ "&&" -> cont 26;
	L.ReservedOp _ "||" -> cont 27;
	L.ReservedOp _ ":=" -> cont 28;
	L.ReservedOp _ "=" -> cont 29;
	L.Reserved _ "fn" -> cont 30;
	L.Reserved _ "if" -> cont 31;
	L.Reserved _ "let" -> cont 32;
	L.Reserved _ "else" -> cont 33;
	L.Reserved _ "true" -> cont 34;
	L.Reserved _ "false" -> cont 35;
	L.Reserved _ "while" -> cont 36;
	L.Reserved _ "return" -> cont 37;
	L.Int _ _ -> cont 38;
	L.Ident _ _ -> cont 39;
	L.String _ _ -> cont 40;
	L.Sym _ '(' -> cont 41;
	L.Sym _ ')' -> cont 42;
	L.Sym _ '{' -> cont 43;
	L.Sym _ '}' -> cont 44;
	L.Sym _ '[' -> cont 45;
	L.Sym _ ']' -> cont 46;
	L.Sym _ ',' -> cont 47;
	L.Sym _ ';' -> cont 48;
	_ -> happyError' (tk:tks)
	}

happyError_ 49 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(L.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseTokens tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.Token] -> a
parseError (x:_) =
    error $ "parser error: " ++ show x
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 10 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc9827_0/ghc_2.h" #-}


































































































































































{-# LINE 10 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
