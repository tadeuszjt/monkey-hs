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

action_0 (30) = happyShift action_8
action_0 (31) = happyShift action_9
action_0 (32) = happyShift action_10
action_0 (34) = happyShift action_11
action_0 (35) = happyShift action_12
action_0 (36) = happyShift action_13
action_0 (37) = happyShift action_14
action_0 (38) = happyShift action_15
action_0 (39) = happyShift action_16
action_0 (40) = happyShift action_17
action_0 (41) = happyShift action_18
action_0 (42) = happyShift action_19
action_0 (44) = happyShift action_20
action_0 (46) = happyShift action_21
action_0 (4) = happyGoto action_22
action_0 (5) = happyGoto action_23
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (9) = happyGoto action_5
action_0 (11) = happyGoto action_6
action_0 (12) = happyGoto action_7
action_0 _ = happyFail

action_1 (30) = happyShift action_8
action_1 (31) = happyShift action_9
action_1 (32) = happyShift action_10
action_1 (34) = happyShift action_11
action_1 (35) = happyShift action_12
action_1 (36) = happyShift action_13
action_1 (37) = happyShift action_14
action_1 (38) = happyShift action_15
action_1 (39) = happyShift action_16
action_1 (40) = happyShift action_17
action_1 (41) = happyShift action_18
action_1 (42) = happyShift action_19
action_1 (44) = happyShift action_20
action_1 (46) = happyShift action_21
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (9) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 (12) = happyGoto action_7
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (49) = happyShift action_52
action_3 _ = happyFail

action_4 _ = happyReduce_5

action_5 _ = happyReduce_6

action_6 (15) = happyShift action_39
action_6 (16) = happyShift action_40
action_6 (17) = happyShift action_41
action_6 (18) = happyShift action_42
action_6 (19) = happyShift action_43
action_6 (20) = happyShift action_44
action_6 (21) = happyShift action_45
action_6 (22) = happyShift action_46
action_6 (23) = happyShift action_47
action_6 (24) = happyShift action_48
action_6 (27) = happyShift action_49
action_6 (42) = happyShift action_50
action_6 (46) = happyShift action_51
action_6 _ = happyReduce_10

action_7 _ = happyReduce_25

action_8 (42) = happyShift action_38
action_8 _ = happyFail

action_9 (30) = happyShift action_8
action_9 (34) = happyShift action_11
action_9 (35) = happyShift action_12
action_9 (38) = happyShift action_15
action_9 (39) = happyShift action_16
action_9 (40) = happyShift action_27
action_9 (41) = happyShift action_18
action_9 (42) = happyShift action_19
action_9 (46) = happyShift action_21
action_9 (11) = happyGoto action_37
action_9 (12) = happyGoto action_7
action_9 _ = happyFail

action_10 (40) = happyShift action_36
action_10 _ = happyFail

action_11 _ = happyReduce_20

action_12 _ = happyReduce_21

action_13 (30) = happyShift action_8
action_13 (34) = happyShift action_11
action_13 (35) = happyShift action_12
action_13 (38) = happyShift action_15
action_13 (39) = happyShift action_16
action_13 (40) = happyShift action_27
action_13 (41) = happyShift action_18
action_13 (42) = happyShift action_19
action_13 (46) = happyShift action_21
action_13 (11) = happyGoto action_35
action_13 (12) = happyGoto action_7
action_13 _ = happyFail

action_14 (30) = happyShift action_8
action_14 (34) = happyShift action_11
action_14 (35) = happyShift action_12
action_14 (38) = happyShift action_15
action_14 (39) = happyShift action_16
action_14 (40) = happyShift action_27
action_14 (41) = happyShift action_18
action_14 (42) = happyShift action_19
action_14 (46) = happyShift action_21
action_14 (11) = happyGoto action_34
action_14 (12) = happyGoto action_7
action_14 _ = happyFail

action_15 (42) = happyShift action_33
action_15 _ = happyFail

action_16 _ = happyReduce_19

action_17 (29) = happyShift action_32
action_17 _ = happyReduce_24

action_18 _ = happyReduce_23

action_19 (30) = happyShift action_8
action_19 (34) = happyShift action_11
action_19 (35) = happyShift action_12
action_19 (38) = happyShift action_15
action_19 (39) = happyShift action_16
action_19 (40) = happyShift action_27
action_19 (41) = happyShift action_18
action_19 (42) = happyShift action_19
action_19 (46) = happyShift action_21
action_19 (11) = happyGoto action_31
action_19 (12) = happyGoto action_7
action_19 _ = happyFail

action_20 (30) = happyShift action_8
action_20 (31) = happyShift action_9
action_20 (32) = happyShift action_10
action_20 (34) = happyShift action_11
action_20 (35) = happyShift action_12
action_20 (36) = happyShift action_13
action_20 (37) = happyShift action_14
action_20 (38) = happyShift action_15
action_20 (39) = happyShift action_16
action_20 (40) = happyShift action_17
action_20 (41) = happyShift action_18
action_20 (42) = happyShift action_19
action_20 (44) = happyShift action_20
action_20 (46) = happyShift action_21
action_20 (5) = happyGoto action_28
action_20 (6) = happyGoto action_3
action_20 (7) = happyGoto action_4
action_20 (9) = happyGoto action_5
action_20 (10) = happyGoto action_29
action_20 (11) = happyGoto action_30
action_20 (12) = happyGoto action_7
action_20 _ = happyReduce_17

action_21 (30) = happyShift action_8
action_21 (34) = happyShift action_11
action_21 (35) = happyShift action_12
action_21 (38) = happyShift action_15
action_21 (39) = happyShift action_16
action_21 (40) = happyShift action_27
action_21 (41) = happyShift action_18
action_21 (42) = happyShift action_19
action_21 (46) = happyShift action_21
action_21 (11) = happyGoto action_25
action_21 (12) = happyGoto action_7
action_21 (13) = happyGoto action_26
action_21 _ = happyReduce_42

action_22 (50) = happyAccept
action_22 _ = happyFail

action_23 (30) = happyShift action_8
action_23 (31) = happyShift action_9
action_23 (32) = happyShift action_10
action_23 (34) = happyShift action_11
action_23 (35) = happyShift action_12
action_23 (36) = happyShift action_13
action_23 (37) = happyShift action_14
action_23 (38) = happyShift action_15
action_23 (39) = happyShift action_16
action_23 (40) = happyShift action_17
action_23 (41) = happyShift action_18
action_23 (42) = happyShift action_19
action_23 (44) = happyShift action_20
action_23 (46) = happyShift action_21
action_23 (4) = happyGoto action_24
action_23 (5) = happyGoto action_23
action_23 (6) = happyGoto action_3
action_23 (7) = happyGoto action_4
action_23 (9) = happyGoto action_5
action_23 (11) = happyGoto action_6
action_23 (12) = happyGoto action_7
action_23 _ = happyReduce_1

action_24 _ = happyReduce_2

action_25 (15) = happyShift action_39
action_25 (16) = happyShift action_40
action_25 (17) = happyShift action_41
action_25 (18) = happyShift action_42
action_25 (19) = happyShift action_43
action_25 (20) = happyShift action_44
action_25 (21) = happyShift action_45
action_25 (22) = happyShift action_46
action_25 (23) = happyShift action_47
action_25 (24) = happyShift action_48
action_25 (27) = happyShift action_49
action_25 (42) = happyShift action_50
action_25 (46) = happyShift action_51
action_25 (48) = happyShift action_78
action_25 _ = happyReduce_43

action_26 (47) = happyShift action_77
action_26 _ = happyFail

action_27 _ = happyReduce_24

action_28 (30) = happyShift action_8
action_28 (31) = happyShift action_9
action_28 (32) = happyShift action_10
action_28 (34) = happyShift action_11
action_28 (35) = happyShift action_12
action_28 (36) = happyShift action_13
action_28 (37) = happyShift action_14
action_28 (38) = happyShift action_15
action_28 (39) = happyShift action_16
action_28 (40) = happyShift action_17
action_28 (41) = happyShift action_18
action_28 (42) = happyShift action_19
action_28 (44) = happyShift action_20
action_28 (46) = happyShift action_21
action_28 (5) = happyGoto action_28
action_28 (6) = happyGoto action_3
action_28 (7) = happyGoto action_4
action_28 (9) = happyGoto action_5
action_28 (10) = happyGoto action_76
action_28 (11) = happyGoto action_6
action_28 (12) = happyGoto action_7
action_28 _ = happyReduce_17

action_29 (45) = happyShift action_75
action_29 _ = happyFail

action_30 (15) = happyShift action_39
action_30 (16) = happyShift action_40
action_30 (17) = happyShift action_41
action_30 (18) = happyShift action_42
action_30 (19) = happyShift action_43
action_30 (20) = happyShift action_44
action_30 (21) = happyShift action_45
action_30 (22) = happyShift action_46
action_30 (23) = happyShift action_47
action_30 (24) = happyShift action_48
action_30 (27) = happyShift action_49
action_30 (42) = happyShift action_50
action_30 (45) = happyShift action_74
action_30 (46) = happyShift action_51
action_30 _ = happyReduce_10

action_31 (15) = happyShift action_39
action_31 (16) = happyShift action_40
action_31 (17) = happyShift action_41
action_31 (18) = happyShift action_42
action_31 (19) = happyShift action_43
action_31 (20) = happyShift action_44
action_31 (21) = happyShift action_45
action_31 (22) = happyShift action_46
action_31 (23) = happyShift action_47
action_31 (24) = happyShift action_48
action_31 (27) = happyShift action_49
action_31 (42) = happyShift action_50
action_31 (43) = happyShift action_73
action_31 (46) = happyShift action_51
action_31 _ = happyFail

action_32 (30) = happyShift action_8
action_32 (34) = happyShift action_11
action_32 (35) = happyShift action_12
action_32 (38) = happyShift action_15
action_32 (39) = happyShift action_16
action_32 (40) = happyShift action_27
action_32 (41) = happyShift action_18
action_32 (42) = happyShift action_19
action_32 (46) = happyShift action_21
action_32 (11) = happyGoto action_72
action_32 (12) = happyGoto action_7
action_32 _ = happyFail

action_33 (30) = happyShift action_8
action_33 (34) = happyShift action_11
action_33 (35) = happyShift action_12
action_33 (38) = happyShift action_15
action_33 (39) = happyShift action_16
action_33 (40) = happyShift action_27
action_33 (41) = happyShift action_18
action_33 (42) = happyShift action_19
action_33 (46) = happyShift action_21
action_33 (11) = happyGoto action_25
action_33 (12) = happyGoto action_7
action_33 (13) = happyGoto action_71
action_33 _ = happyReduce_42

action_34 (15) = happyShift action_39
action_34 (16) = happyShift action_40
action_34 (17) = happyShift action_41
action_34 (18) = happyShift action_42
action_34 (19) = happyShift action_43
action_34 (20) = happyShift action_44
action_34 (21) = happyShift action_45
action_34 (22) = happyShift action_46
action_34 (23) = happyShift action_47
action_34 (24) = happyShift action_48
action_34 (27) = happyShift action_49
action_34 (42) = happyShift action_50
action_34 (46) = happyShift action_51
action_34 _ = happyReduce_9

action_35 (15) = happyShift action_39
action_35 (16) = happyShift action_40
action_35 (17) = happyShift action_41
action_35 (18) = happyShift action_42
action_35 (19) = happyShift action_43
action_35 (20) = happyShift action_44
action_35 (21) = happyShift action_45
action_35 (22) = happyShift action_46
action_35 (23) = happyShift action_47
action_35 (24) = happyShift action_48
action_35 (27) = happyShift action_49
action_35 (42) = happyShift action_50
action_35 (44) = happyShift action_20
action_35 (46) = happyShift action_51
action_35 (9) = happyGoto action_70
action_35 _ = happyFail

action_36 (29) = happyShift action_69
action_36 _ = happyFail

action_37 (15) = happyShift action_39
action_37 (16) = happyShift action_40
action_37 (17) = happyShift action_41
action_37 (18) = happyShift action_42
action_37 (19) = happyShift action_43
action_37 (20) = happyShift action_44
action_37 (21) = happyShift action_45
action_37 (22) = happyShift action_46
action_37 (23) = happyShift action_47
action_37 (24) = happyShift action_48
action_37 (27) = happyShift action_49
action_37 (42) = happyShift action_50
action_37 (44) = happyShift action_20
action_37 (46) = happyShift action_51
action_37 (9) = happyGoto action_68
action_37 _ = happyFail

action_38 (40) = happyShift action_67
action_38 (14) = happyGoto action_66
action_38 _ = happyReduce_45

action_39 (30) = happyShift action_8
action_39 (34) = happyShift action_11
action_39 (35) = happyShift action_12
action_39 (38) = happyShift action_15
action_39 (39) = happyShift action_16
action_39 (40) = happyShift action_27
action_39 (41) = happyShift action_18
action_39 (42) = happyShift action_19
action_39 (46) = happyShift action_21
action_39 (11) = happyGoto action_65
action_39 (12) = happyGoto action_7
action_39 _ = happyFail

action_40 (30) = happyShift action_8
action_40 (34) = happyShift action_11
action_40 (35) = happyShift action_12
action_40 (38) = happyShift action_15
action_40 (39) = happyShift action_16
action_40 (40) = happyShift action_27
action_40 (41) = happyShift action_18
action_40 (42) = happyShift action_19
action_40 (46) = happyShift action_21
action_40 (11) = happyGoto action_64
action_40 (12) = happyGoto action_7
action_40 _ = happyFail

action_41 (30) = happyShift action_8
action_41 (34) = happyShift action_11
action_41 (35) = happyShift action_12
action_41 (38) = happyShift action_15
action_41 (39) = happyShift action_16
action_41 (40) = happyShift action_27
action_41 (41) = happyShift action_18
action_41 (42) = happyShift action_19
action_41 (46) = happyShift action_21
action_41 (11) = happyGoto action_63
action_41 (12) = happyGoto action_7
action_41 _ = happyFail

action_42 (30) = happyShift action_8
action_42 (34) = happyShift action_11
action_42 (35) = happyShift action_12
action_42 (38) = happyShift action_15
action_42 (39) = happyShift action_16
action_42 (40) = happyShift action_27
action_42 (41) = happyShift action_18
action_42 (42) = happyShift action_19
action_42 (46) = happyShift action_21
action_42 (11) = happyGoto action_62
action_42 (12) = happyGoto action_7
action_42 _ = happyFail

action_43 (30) = happyShift action_8
action_43 (34) = happyShift action_11
action_43 (35) = happyShift action_12
action_43 (38) = happyShift action_15
action_43 (39) = happyShift action_16
action_43 (40) = happyShift action_27
action_43 (41) = happyShift action_18
action_43 (42) = happyShift action_19
action_43 (46) = happyShift action_21
action_43 (11) = happyGoto action_61
action_43 (12) = happyGoto action_7
action_43 _ = happyFail

action_44 (30) = happyShift action_8
action_44 (34) = happyShift action_11
action_44 (35) = happyShift action_12
action_44 (38) = happyShift action_15
action_44 (39) = happyShift action_16
action_44 (40) = happyShift action_27
action_44 (41) = happyShift action_18
action_44 (42) = happyShift action_19
action_44 (46) = happyShift action_21
action_44 (11) = happyGoto action_60
action_44 (12) = happyGoto action_7
action_44 _ = happyFail

action_45 (30) = happyShift action_8
action_45 (34) = happyShift action_11
action_45 (35) = happyShift action_12
action_45 (38) = happyShift action_15
action_45 (39) = happyShift action_16
action_45 (40) = happyShift action_27
action_45 (41) = happyShift action_18
action_45 (42) = happyShift action_19
action_45 (46) = happyShift action_21
action_45 (11) = happyGoto action_59
action_45 (12) = happyGoto action_7
action_45 _ = happyFail

action_46 (30) = happyShift action_8
action_46 (34) = happyShift action_11
action_46 (35) = happyShift action_12
action_46 (38) = happyShift action_15
action_46 (39) = happyShift action_16
action_46 (40) = happyShift action_27
action_46 (41) = happyShift action_18
action_46 (42) = happyShift action_19
action_46 (46) = happyShift action_21
action_46 (11) = happyGoto action_58
action_46 (12) = happyGoto action_7
action_46 _ = happyFail

action_47 (30) = happyShift action_8
action_47 (34) = happyShift action_11
action_47 (35) = happyShift action_12
action_47 (38) = happyShift action_15
action_47 (39) = happyShift action_16
action_47 (40) = happyShift action_27
action_47 (41) = happyShift action_18
action_47 (42) = happyShift action_19
action_47 (46) = happyShift action_21
action_47 (11) = happyGoto action_57
action_47 (12) = happyGoto action_7
action_47 _ = happyFail

action_48 (30) = happyShift action_8
action_48 (34) = happyShift action_11
action_48 (35) = happyShift action_12
action_48 (38) = happyShift action_15
action_48 (39) = happyShift action_16
action_48 (40) = happyShift action_27
action_48 (41) = happyShift action_18
action_48 (42) = happyShift action_19
action_48 (46) = happyShift action_21
action_48 (11) = happyGoto action_56
action_48 (12) = happyGoto action_7
action_48 _ = happyFail

action_49 (30) = happyShift action_8
action_49 (34) = happyShift action_11
action_49 (35) = happyShift action_12
action_49 (38) = happyShift action_15
action_49 (39) = happyShift action_16
action_49 (40) = happyShift action_27
action_49 (41) = happyShift action_18
action_49 (42) = happyShift action_19
action_49 (46) = happyShift action_21
action_49 (11) = happyGoto action_55
action_49 (12) = happyGoto action_7
action_49 _ = happyFail

action_50 (30) = happyShift action_8
action_50 (34) = happyShift action_11
action_50 (35) = happyShift action_12
action_50 (38) = happyShift action_15
action_50 (39) = happyShift action_16
action_50 (40) = happyShift action_27
action_50 (41) = happyShift action_18
action_50 (42) = happyShift action_19
action_50 (46) = happyShift action_21
action_50 (11) = happyGoto action_25
action_50 (12) = happyGoto action_7
action_50 (13) = happyGoto action_54
action_50 _ = happyReduce_42

action_51 (30) = happyShift action_8
action_51 (34) = happyShift action_11
action_51 (35) = happyShift action_12
action_51 (38) = happyShift action_15
action_51 (39) = happyShift action_16
action_51 (40) = happyShift action_27
action_51 (41) = happyShift action_18
action_51 (42) = happyShift action_19
action_51 (46) = happyShift action_21
action_51 (11) = happyGoto action_53
action_51 (12) = happyGoto action_7
action_51 _ = happyFail

action_52 _ = happyReduce_3

action_53 (15) = happyShift action_39
action_53 (16) = happyShift action_40
action_53 (17) = happyShift action_41
action_53 (18) = happyShift action_42
action_53 (19) = happyShift action_43
action_53 (20) = happyShift action_44
action_53 (21) = happyShift action_45
action_53 (22) = happyShift action_46
action_53 (23) = happyShift action_47
action_53 (24) = happyShift action_48
action_53 (27) = happyShift action_49
action_53 (42) = happyShift action_50
action_53 (46) = happyShift action_51
action_53 (47) = happyShift action_87
action_53 _ = happyFail

action_54 (43) = happyShift action_86
action_54 _ = happyFail

action_55 (15) = happyShift action_39
action_55 (16) = happyShift action_40
action_55 (17) = happyShift action_41
action_55 (18) = happyShift action_42
action_55 (19) = happyShift action_43
action_55 (20) = happyShift action_44
action_55 (21) = happyShift action_45
action_55 (22) = happyShift action_46
action_55 (23) = happyShift action_47
action_55 (24) = happyShift action_48
action_55 (42) = happyShift action_50
action_55 (46) = happyShift action_51
action_55 _ = happyReduce_40

action_56 (15) = happyShift action_39
action_56 (16) = happyShift action_40
action_56 (17) = happyShift action_41
action_56 (18) = happyShift action_42
action_56 (19) = happyShift action_43
action_56 (20) = happyShift action_44
action_56 (21) = happyShift action_45
action_56 (22) = happyShift action_46
action_56 (23) = happyShift action_47
action_56 (42) = happyShift action_50
action_56 (46) = happyShift action_51
action_56 _ = happyReduce_39

action_57 (22) = happyFail
action_57 (23) = happyFail
action_57 (42) = happyShift action_50
action_57 (46) = happyShift action_51
action_57 _ = happyReduce_38

action_58 (22) = happyFail
action_58 (23) = happyFail
action_58 (42) = happyShift action_50
action_58 (46) = happyShift action_51
action_58 _ = happyReduce_37

action_59 (20) = happyFail
action_59 (21) = happyFail
action_59 (22) = happyShift action_46
action_59 (23) = happyShift action_47
action_59 (42) = happyShift action_50
action_59 (46) = happyShift action_51
action_59 _ = happyReduce_36

action_60 (20) = happyFail
action_60 (21) = happyFail
action_60 (22) = happyShift action_46
action_60 (23) = happyShift action_47
action_60 (42) = happyShift action_50
action_60 (46) = happyShift action_51
action_60 _ = happyReduce_35

action_61 (15) = happyShift action_39
action_61 (16) = happyShift action_40
action_61 (17) = happyShift action_41
action_61 (20) = happyShift action_44
action_61 (21) = happyShift action_45
action_61 (22) = happyShift action_46
action_61 (23) = happyShift action_47
action_61 (42) = happyShift action_50
action_61 (46) = happyShift action_51
action_61 _ = happyReduce_31

action_62 (15) = happyShift action_39
action_62 (16) = happyShift action_40
action_62 (17) = happyShift action_41
action_62 (20) = happyShift action_44
action_62 (21) = happyShift action_45
action_62 (22) = happyShift action_46
action_62 (23) = happyShift action_47
action_62 (42) = happyShift action_50
action_62 (46) = happyShift action_51
action_62 _ = happyReduce_30

action_63 (20) = happyShift action_44
action_63 (21) = happyShift action_45
action_63 (22) = happyShift action_46
action_63 (23) = happyShift action_47
action_63 (42) = happyShift action_50
action_63 (46) = happyShift action_51
action_63 _ = happyReduce_34

action_64 (20) = happyShift action_44
action_64 (21) = happyShift action_45
action_64 (22) = happyShift action_46
action_64 (23) = happyShift action_47
action_64 (42) = happyShift action_50
action_64 (46) = happyShift action_51
action_64 _ = happyReduce_33

action_65 (20) = happyShift action_44
action_65 (21) = happyShift action_45
action_65 (22) = happyShift action_46
action_65 (23) = happyShift action_47
action_65 (42) = happyShift action_50
action_65 (46) = happyShift action_51
action_65 _ = happyReduce_32

action_66 (43) = happyShift action_85
action_66 _ = happyFail

action_67 (48) = happyShift action_84
action_67 _ = happyReduce_46

action_68 (33) = happyShift action_83
action_68 (8) = happyGoto action_82
action_68 _ = happyReduce_12

action_69 (30) = happyShift action_8
action_69 (34) = happyShift action_11
action_69 (35) = happyShift action_12
action_69 (38) = happyShift action_15
action_69 (39) = happyShift action_16
action_69 (40) = happyShift action_27
action_69 (41) = happyShift action_18
action_69 (42) = happyShift action_19
action_69 (46) = happyShift action_21
action_69 (11) = happyGoto action_81
action_69 (12) = happyGoto action_7
action_69 _ = happyFail

action_70 _ = happyReduce_4

action_71 (43) = happyShift action_80
action_71 _ = happyFail

action_72 (15) = happyShift action_39
action_72 (16) = happyShift action_40
action_72 (17) = happyShift action_41
action_72 (18) = happyShift action_42
action_72 (19) = happyShift action_43
action_72 (20) = happyShift action_44
action_72 (21) = happyShift action_45
action_72 (22) = happyShift action_46
action_72 (23) = happyShift action_47
action_72 (24) = happyShift action_48
action_72 (27) = happyShift action_49
action_72 (42) = happyShift action_50
action_72 (46) = happyShift action_51
action_72 _ = happyReduce_8

action_73 _ = happyReduce_26

action_74 _ = happyReduce_16

action_75 _ = happyReduce_15

action_76 _ = happyReduce_18

action_77 _ = happyReduce_41

action_78 (30) = happyShift action_8
action_78 (34) = happyShift action_11
action_78 (35) = happyShift action_12
action_78 (38) = happyShift action_15
action_78 (39) = happyShift action_16
action_78 (40) = happyShift action_27
action_78 (41) = happyShift action_18
action_78 (42) = happyShift action_19
action_78 (46) = happyShift action_21
action_78 (11) = happyGoto action_25
action_78 (12) = happyGoto action_7
action_78 (13) = happyGoto action_79
action_78 _ = happyReduce_42

action_79 _ = happyReduce_44

action_80 _ = happyReduce_29

action_81 (15) = happyShift action_39
action_81 (16) = happyShift action_40
action_81 (17) = happyShift action_41
action_81 (18) = happyShift action_42
action_81 (19) = happyShift action_43
action_81 (20) = happyShift action_44
action_81 (21) = happyShift action_45
action_81 (22) = happyShift action_46
action_81 (23) = happyShift action_47
action_81 (24) = happyShift action_48
action_81 (27) = happyShift action_49
action_81 (42) = happyShift action_50
action_81 (46) = happyShift action_51
action_81 _ = happyReduce_7

action_82 _ = happyReduce_11

action_83 (31) = happyShift action_9
action_83 (44) = happyShift action_20
action_83 (7) = happyGoto action_90
action_83 (9) = happyGoto action_91
action_83 _ = happyFail

action_84 (40) = happyShift action_67
action_84 (14) = happyGoto action_89
action_84 _ = happyReduce_45

action_85 (44) = happyShift action_20
action_85 (9) = happyGoto action_88
action_85 _ = happyFail

action_86 _ = happyReduce_28

action_87 _ = happyReduce_27

action_88 _ = happyReduce_22

action_89 _ = happyReduce_47

action_90 _ = happyReduce_14

action_91 _ = happyReduce_13

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

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (let (L.Ident _ s) = happy_var_2 in S.Assign (L.tokPosn happy_var_1) s happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (let (L.Ident _ s) = happy_var_1 in S.Set (L.tokPosn happy_var_2) s happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn6
		 (S.Return (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn6
		 (S.ExprStmt happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (S.If (L.tokPosn happy_var_1) happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_0  8 happyReduction_12
happyReduction_12  =  HappyAbsSyn8
		 (Nothing
	)

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Just happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  8 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Just happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (S.Block happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (S.BlockExpr happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  10 happyReduction_17
happyReduction_17  =  HappyAbsSyn10
		 ([]
	)

happyReduce_18 = happySpecReduce_2  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let (L.Int p i) = happy_var_1 in S.Int p i
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (S.Bool (L.tokPosn happy_var_1) True
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (S.Bool (L.tokPosn happy_var_1) False
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 11 happyReduction_22
happyReduction_22 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Func (L.tokPosn happy_var_1) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (let (L.String p s) = happy_var_1 in S.String p s
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 ((\(L.Ident p s) -> S.Ident p s) happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  11 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 11 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Subscript (S.exprPosn happy_var_3) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 11 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Call  (L.tokPosn happy_var_2) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 11 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Call  (L.tokPosn happy_var_2) (S.Ident (L.tokPosn happy_var_1) "print") happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  11 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Plus happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  11 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Minus happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  11 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Times happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  11 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Divide happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  11 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Mod happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  11 happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.LT happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  11 happyReduction_36
happyReduction_36 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.GT happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  11 happyReduction_37
happyReduction_37 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.LTEq happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  11 happyReduction_38
happyReduction_38 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.GTEq happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  11 happyReduction_39
happyReduction_39 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.EqEq happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  11 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.OrOr happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  12 happyReduction_41
happyReduction_41 _
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (S.Array (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  13 happyReduction_42
happyReduction_42  =  HappyAbsSyn13
		 ([]
	)

happyReduce_43 = happySpecReduce_1  13 happyReduction_43
happyReduction_43 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  13 happyReduction_44
happyReduction_44 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  14 happyReduction_45
happyReduction_45  =  HappyAbsSyn14
		 ([]
	)

happyReduce_46 = happySpecReduce_1  14 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (let (L.Ident p s) = happy_var_1 in [s]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  14 happyReduction_47
happyReduction_47 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (let (L.Ident p s) = happy_var_1 in s : happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

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
	L.Reserved _ "print" -> cont 38;
	L.Int _ _ -> cont 39;
	L.Ident _ _ -> cont 40;
	L.String _ _ -> cont 41;
	L.Sym _ '(' -> cont 42;
	L.Sym _ ')' -> cont 43;
	L.Sym _ '{' -> cont 44;
	L.Sym _ '}' -> cont 45;
	L.Sym _ '[' -> cont 46;
	L.Sym _ ']' -> cont 47;
	L.Sym _ ',' -> cont 48;
	L.Sym _ ';' -> cont 49;
	_ -> happyError' (tk:tks)
	}

happyError_ 50 tk tks = happyError' tks
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
