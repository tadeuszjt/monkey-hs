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
action_0 (42) = happyShift action_18
action_0 (46) = happyShift action_19
action_0 (4) = happyGoto action_20
action_0 (5) = happyGoto action_21
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
action_1 (42) = happyShift action_18
action_1 (46) = happyShift action_19
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 (12) = happyGoto action_6
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (49) = happyShift action_47
action_3 _ = happyFail

action_4 _ = happyReduce_5

action_5 (15) = happyShift action_34
action_5 (16) = happyShift action_35
action_5 (17) = happyShift action_36
action_5 (18) = happyShift action_37
action_5 (19) = happyShift action_38
action_5 (20) = happyShift action_39
action_5 (21) = happyShift action_40
action_5 (22) = happyShift action_41
action_5 (23) = happyShift action_42
action_5 (24) = happyShift action_43
action_5 (27) = happyShift action_44
action_5 (42) = happyShift action_45
action_5 (46) = happyShift action_46
action_5 _ = happyReduce_9

action_6 _ = happyReduce_24

action_7 (42) = happyShift action_33
action_7 _ = happyFail

action_8 (30) = happyShift action_7
action_8 (34) = happyShift action_10
action_8 (35) = happyShift action_11
action_8 (38) = happyShift action_14
action_8 (39) = happyShift action_15
action_8 (40) = happyShift action_25
action_8 (41) = happyShift action_17
action_8 (42) = happyShift action_18
action_8 (46) = happyShift action_19
action_8 (11) = happyGoto action_32
action_8 (12) = happyGoto action_6
action_8 _ = happyFail

action_9 (40) = happyShift action_31
action_9 _ = happyFail

action_10 _ = happyReduce_19

action_11 _ = happyReduce_20

action_12 (30) = happyShift action_7
action_12 (34) = happyShift action_10
action_12 (35) = happyShift action_11
action_12 (38) = happyShift action_14
action_12 (39) = happyShift action_15
action_12 (40) = happyShift action_25
action_12 (41) = happyShift action_17
action_12 (42) = happyShift action_18
action_12 (46) = happyShift action_19
action_12 (11) = happyGoto action_30
action_12 (12) = happyGoto action_6
action_12 _ = happyFail

action_13 (30) = happyShift action_7
action_13 (34) = happyShift action_10
action_13 (35) = happyShift action_11
action_13 (38) = happyShift action_14
action_13 (39) = happyShift action_15
action_13 (40) = happyShift action_25
action_13 (41) = happyShift action_17
action_13 (42) = happyShift action_18
action_13 (46) = happyShift action_19
action_13 (11) = happyGoto action_29
action_13 (12) = happyGoto action_6
action_13 _ = happyFail

action_14 (42) = happyShift action_28
action_14 _ = happyFail

action_15 _ = happyReduce_18

action_16 (29) = happyShift action_27
action_16 _ = happyReduce_23

action_17 _ = happyReduce_22

action_18 (30) = happyShift action_7
action_18 (34) = happyShift action_10
action_18 (35) = happyShift action_11
action_18 (38) = happyShift action_14
action_18 (39) = happyShift action_15
action_18 (40) = happyShift action_25
action_18 (41) = happyShift action_17
action_18 (42) = happyShift action_18
action_18 (46) = happyShift action_19
action_18 (11) = happyGoto action_26
action_18 (12) = happyGoto action_6
action_18 _ = happyFail

action_19 (30) = happyShift action_7
action_19 (34) = happyShift action_10
action_19 (35) = happyShift action_11
action_19 (38) = happyShift action_14
action_19 (39) = happyShift action_15
action_19 (40) = happyShift action_25
action_19 (41) = happyShift action_17
action_19 (42) = happyShift action_18
action_19 (46) = happyShift action_19
action_19 (11) = happyGoto action_23
action_19 (12) = happyGoto action_6
action_19 (13) = happyGoto action_24
action_19 _ = happyReduce_41

action_20 (50) = happyAccept
action_20 _ = happyFail

action_21 (30) = happyShift action_7
action_21 (31) = happyShift action_8
action_21 (32) = happyShift action_9
action_21 (34) = happyShift action_10
action_21 (35) = happyShift action_11
action_21 (36) = happyShift action_12
action_21 (37) = happyShift action_13
action_21 (38) = happyShift action_14
action_21 (39) = happyShift action_15
action_21 (40) = happyShift action_16
action_21 (41) = happyShift action_17
action_21 (42) = happyShift action_18
action_21 (46) = happyShift action_19
action_21 (4) = happyGoto action_22
action_21 (5) = happyGoto action_21
action_21 (6) = happyGoto action_3
action_21 (7) = happyGoto action_4
action_21 (11) = happyGoto action_5
action_21 (12) = happyGoto action_6
action_21 _ = happyReduce_1

action_22 _ = happyReduce_2

action_23 (15) = happyShift action_34
action_23 (16) = happyShift action_35
action_23 (17) = happyShift action_36
action_23 (18) = happyShift action_37
action_23 (19) = happyShift action_38
action_23 (20) = happyShift action_39
action_23 (21) = happyShift action_40
action_23 (22) = happyShift action_41
action_23 (23) = happyShift action_42
action_23 (24) = happyShift action_43
action_23 (27) = happyShift action_44
action_23 (42) = happyShift action_45
action_23 (46) = happyShift action_46
action_23 (48) = happyShift action_71
action_23 _ = happyReduce_42

action_24 (47) = happyShift action_70
action_24 _ = happyFail

action_25 _ = happyReduce_23

action_26 (15) = happyShift action_34
action_26 (16) = happyShift action_35
action_26 (17) = happyShift action_36
action_26 (18) = happyShift action_37
action_26 (19) = happyShift action_38
action_26 (20) = happyShift action_39
action_26 (21) = happyShift action_40
action_26 (22) = happyShift action_41
action_26 (23) = happyShift action_42
action_26 (24) = happyShift action_43
action_26 (27) = happyShift action_44
action_26 (42) = happyShift action_45
action_26 (43) = happyShift action_69
action_26 (46) = happyShift action_46
action_26 _ = happyFail

action_27 (30) = happyShift action_7
action_27 (34) = happyShift action_10
action_27 (35) = happyShift action_11
action_27 (38) = happyShift action_14
action_27 (39) = happyShift action_15
action_27 (40) = happyShift action_25
action_27 (41) = happyShift action_17
action_27 (42) = happyShift action_18
action_27 (46) = happyShift action_19
action_27 (11) = happyGoto action_68
action_27 (12) = happyGoto action_6
action_27 _ = happyFail

action_28 (30) = happyShift action_7
action_28 (34) = happyShift action_10
action_28 (35) = happyShift action_11
action_28 (38) = happyShift action_14
action_28 (39) = happyShift action_15
action_28 (40) = happyShift action_25
action_28 (41) = happyShift action_17
action_28 (42) = happyShift action_18
action_28 (46) = happyShift action_19
action_28 (11) = happyGoto action_23
action_28 (12) = happyGoto action_6
action_28 (13) = happyGoto action_67
action_28 _ = happyReduce_41

action_29 (15) = happyShift action_34
action_29 (16) = happyShift action_35
action_29 (17) = happyShift action_36
action_29 (18) = happyShift action_37
action_29 (19) = happyShift action_38
action_29 (20) = happyShift action_39
action_29 (21) = happyShift action_40
action_29 (22) = happyShift action_41
action_29 (23) = happyShift action_42
action_29 (24) = happyShift action_43
action_29 (27) = happyShift action_44
action_29 (42) = happyShift action_45
action_29 (46) = happyShift action_46
action_29 _ = happyReduce_8

action_30 (15) = happyShift action_34
action_30 (16) = happyShift action_35
action_30 (17) = happyShift action_36
action_30 (18) = happyShift action_37
action_30 (19) = happyShift action_38
action_30 (20) = happyShift action_39
action_30 (21) = happyShift action_40
action_30 (22) = happyShift action_41
action_30 (23) = happyShift action_42
action_30 (24) = happyShift action_43
action_30 (27) = happyShift action_44
action_30 (42) = happyShift action_45
action_30 (44) = happyShift action_64
action_30 (46) = happyShift action_46
action_30 (9) = happyGoto action_66
action_30 _ = happyFail

action_31 (29) = happyShift action_65
action_31 _ = happyFail

action_32 (15) = happyShift action_34
action_32 (16) = happyShift action_35
action_32 (17) = happyShift action_36
action_32 (18) = happyShift action_37
action_32 (19) = happyShift action_38
action_32 (20) = happyShift action_39
action_32 (21) = happyShift action_40
action_32 (22) = happyShift action_41
action_32 (23) = happyShift action_42
action_32 (24) = happyShift action_43
action_32 (27) = happyShift action_44
action_32 (42) = happyShift action_45
action_32 (44) = happyShift action_64
action_32 (46) = happyShift action_46
action_32 (9) = happyGoto action_63
action_32 _ = happyFail

action_33 (40) = happyShift action_62
action_33 (14) = happyGoto action_61
action_33 _ = happyReduce_44

action_34 (30) = happyShift action_7
action_34 (34) = happyShift action_10
action_34 (35) = happyShift action_11
action_34 (38) = happyShift action_14
action_34 (39) = happyShift action_15
action_34 (40) = happyShift action_25
action_34 (41) = happyShift action_17
action_34 (42) = happyShift action_18
action_34 (46) = happyShift action_19
action_34 (11) = happyGoto action_60
action_34 (12) = happyGoto action_6
action_34 _ = happyFail

action_35 (30) = happyShift action_7
action_35 (34) = happyShift action_10
action_35 (35) = happyShift action_11
action_35 (38) = happyShift action_14
action_35 (39) = happyShift action_15
action_35 (40) = happyShift action_25
action_35 (41) = happyShift action_17
action_35 (42) = happyShift action_18
action_35 (46) = happyShift action_19
action_35 (11) = happyGoto action_59
action_35 (12) = happyGoto action_6
action_35 _ = happyFail

action_36 (30) = happyShift action_7
action_36 (34) = happyShift action_10
action_36 (35) = happyShift action_11
action_36 (38) = happyShift action_14
action_36 (39) = happyShift action_15
action_36 (40) = happyShift action_25
action_36 (41) = happyShift action_17
action_36 (42) = happyShift action_18
action_36 (46) = happyShift action_19
action_36 (11) = happyGoto action_58
action_36 (12) = happyGoto action_6
action_36 _ = happyFail

action_37 (30) = happyShift action_7
action_37 (34) = happyShift action_10
action_37 (35) = happyShift action_11
action_37 (38) = happyShift action_14
action_37 (39) = happyShift action_15
action_37 (40) = happyShift action_25
action_37 (41) = happyShift action_17
action_37 (42) = happyShift action_18
action_37 (46) = happyShift action_19
action_37 (11) = happyGoto action_57
action_37 (12) = happyGoto action_6
action_37 _ = happyFail

action_38 (30) = happyShift action_7
action_38 (34) = happyShift action_10
action_38 (35) = happyShift action_11
action_38 (38) = happyShift action_14
action_38 (39) = happyShift action_15
action_38 (40) = happyShift action_25
action_38 (41) = happyShift action_17
action_38 (42) = happyShift action_18
action_38 (46) = happyShift action_19
action_38 (11) = happyGoto action_56
action_38 (12) = happyGoto action_6
action_38 _ = happyFail

action_39 (30) = happyShift action_7
action_39 (34) = happyShift action_10
action_39 (35) = happyShift action_11
action_39 (38) = happyShift action_14
action_39 (39) = happyShift action_15
action_39 (40) = happyShift action_25
action_39 (41) = happyShift action_17
action_39 (42) = happyShift action_18
action_39 (46) = happyShift action_19
action_39 (11) = happyGoto action_55
action_39 (12) = happyGoto action_6
action_39 _ = happyFail

action_40 (30) = happyShift action_7
action_40 (34) = happyShift action_10
action_40 (35) = happyShift action_11
action_40 (38) = happyShift action_14
action_40 (39) = happyShift action_15
action_40 (40) = happyShift action_25
action_40 (41) = happyShift action_17
action_40 (42) = happyShift action_18
action_40 (46) = happyShift action_19
action_40 (11) = happyGoto action_54
action_40 (12) = happyGoto action_6
action_40 _ = happyFail

action_41 (30) = happyShift action_7
action_41 (34) = happyShift action_10
action_41 (35) = happyShift action_11
action_41 (38) = happyShift action_14
action_41 (39) = happyShift action_15
action_41 (40) = happyShift action_25
action_41 (41) = happyShift action_17
action_41 (42) = happyShift action_18
action_41 (46) = happyShift action_19
action_41 (11) = happyGoto action_53
action_41 (12) = happyGoto action_6
action_41 _ = happyFail

action_42 (30) = happyShift action_7
action_42 (34) = happyShift action_10
action_42 (35) = happyShift action_11
action_42 (38) = happyShift action_14
action_42 (39) = happyShift action_15
action_42 (40) = happyShift action_25
action_42 (41) = happyShift action_17
action_42 (42) = happyShift action_18
action_42 (46) = happyShift action_19
action_42 (11) = happyGoto action_52
action_42 (12) = happyGoto action_6
action_42 _ = happyFail

action_43 (30) = happyShift action_7
action_43 (34) = happyShift action_10
action_43 (35) = happyShift action_11
action_43 (38) = happyShift action_14
action_43 (39) = happyShift action_15
action_43 (40) = happyShift action_25
action_43 (41) = happyShift action_17
action_43 (42) = happyShift action_18
action_43 (46) = happyShift action_19
action_43 (11) = happyGoto action_51
action_43 (12) = happyGoto action_6
action_43 _ = happyFail

action_44 (30) = happyShift action_7
action_44 (34) = happyShift action_10
action_44 (35) = happyShift action_11
action_44 (38) = happyShift action_14
action_44 (39) = happyShift action_15
action_44 (40) = happyShift action_25
action_44 (41) = happyShift action_17
action_44 (42) = happyShift action_18
action_44 (46) = happyShift action_19
action_44 (11) = happyGoto action_50
action_44 (12) = happyGoto action_6
action_44 _ = happyFail

action_45 (30) = happyShift action_7
action_45 (34) = happyShift action_10
action_45 (35) = happyShift action_11
action_45 (38) = happyShift action_14
action_45 (39) = happyShift action_15
action_45 (40) = happyShift action_25
action_45 (41) = happyShift action_17
action_45 (42) = happyShift action_18
action_45 (46) = happyShift action_19
action_45 (11) = happyGoto action_23
action_45 (12) = happyGoto action_6
action_45 (13) = happyGoto action_49
action_45 _ = happyReduce_41

action_46 (30) = happyShift action_7
action_46 (34) = happyShift action_10
action_46 (35) = happyShift action_11
action_46 (38) = happyShift action_14
action_46 (39) = happyShift action_15
action_46 (40) = happyShift action_25
action_46 (41) = happyShift action_17
action_46 (42) = happyShift action_18
action_46 (46) = happyShift action_19
action_46 (11) = happyGoto action_48
action_46 (12) = happyGoto action_6
action_46 _ = happyFail

action_47 _ = happyReduce_3

action_48 (15) = happyShift action_34
action_48 (16) = happyShift action_35
action_48 (17) = happyShift action_36
action_48 (18) = happyShift action_37
action_48 (19) = happyShift action_38
action_48 (20) = happyShift action_39
action_48 (21) = happyShift action_40
action_48 (22) = happyShift action_41
action_48 (23) = happyShift action_42
action_48 (24) = happyShift action_43
action_48 (27) = happyShift action_44
action_48 (42) = happyShift action_45
action_48 (46) = happyShift action_46
action_48 (47) = happyShift action_83
action_48 _ = happyFail

action_49 (43) = happyShift action_82
action_49 _ = happyFail

action_50 (15) = happyShift action_34
action_50 (16) = happyShift action_35
action_50 (17) = happyShift action_36
action_50 (18) = happyShift action_37
action_50 (19) = happyShift action_38
action_50 (20) = happyShift action_39
action_50 (21) = happyShift action_40
action_50 (22) = happyShift action_41
action_50 (23) = happyShift action_42
action_50 (24) = happyShift action_43
action_50 (42) = happyShift action_45
action_50 (46) = happyShift action_46
action_50 _ = happyReduce_39

action_51 (15) = happyShift action_34
action_51 (16) = happyShift action_35
action_51 (17) = happyShift action_36
action_51 (18) = happyShift action_37
action_51 (19) = happyShift action_38
action_51 (20) = happyShift action_39
action_51 (21) = happyShift action_40
action_51 (22) = happyShift action_41
action_51 (23) = happyShift action_42
action_51 (42) = happyShift action_45
action_51 (46) = happyShift action_46
action_51 _ = happyReduce_38

action_52 (22) = happyFail
action_52 (23) = happyFail
action_52 (42) = happyShift action_45
action_52 (46) = happyShift action_46
action_52 _ = happyReduce_37

action_53 (22) = happyFail
action_53 (23) = happyFail
action_53 (42) = happyShift action_45
action_53 (46) = happyShift action_46
action_53 _ = happyReduce_36

action_54 (20) = happyFail
action_54 (21) = happyFail
action_54 (22) = happyShift action_41
action_54 (23) = happyShift action_42
action_54 (42) = happyShift action_45
action_54 (46) = happyShift action_46
action_54 _ = happyReduce_35

action_55 (20) = happyFail
action_55 (21) = happyFail
action_55 (22) = happyShift action_41
action_55 (23) = happyShift action_42
action_55 (42) = happyShift action_45
action_55 (46) = happyShift action_46
action_55 _ = happyReduce_34

action_56 (15) = happyShift action_34
action_56 (16) = happyShift action_35
action_56 (17) = happyShift action_36
action_56 (20) = happyShift action_39
action_56 (21) = happyShift action_40
action_56 (22) = happyShift action_41
action_56 (23) = happyShift action_42
action_56 (42) = happyShift action_45
action_56 (46) = happyShift action_46
action_56 _ = happyReduce_30

action_57 (15) = happyShift action_34
action_57 (16) = happyShift action_35
action_57 (17) = happyShift action_36
action_57 (20) = happyShift action_39
action_57 (21) = happyShift action_40
action_57 (22) = happyShift action_41
action_57 (23) = happyShift action_42
action_57 (42) = happyShift action_45
action_57 (46) = happyShift action_46
action_57 _ = happyReduce_29

action_58 (20) = happyShift action_39
action_58 (21) = happyShift action_40
action_58 (22) = happyShift action_41
action_58 (23) = happyShift action_42
action_58 (42) = happyShift action_45
action_58 (46) = happyShift action_46
action_58 _ = happyReduce_33

action_59 (20) = happyShift action_39
action_59 (21) = happyShift action_40
action_59 (22) = happyShift action_41
action_59 (23) = happyShift action_42
action_59 (42) = happyShift action_45
action_59 (46) = happyShift action_46
action_59 _ = happyReduce_32

action_60 (20) = happyShift action_39
action_60 (21) = happyShift action_40
action_60 (22) = happyShift action_41
action_60 (23) = happyShift action_42
action_60 (42) = happyShift action_45
action_60 (46) = happyShift action_46
action_60 _ = happyReduce_31

action_61 (43) = happyShift action_81
action_61 _ = happyFail

action_62 (48) = happyShift action_80
action_62 _ = happyReduce_45

action_63 (33) = happyShift action_79
action_63 (8) = happyGoto action_78
action_63 _ = happyReduce_11

action_64 (30) = happyShift action_7
action_64 (31) = happyShift action_8
action_64 (32) = happyShift action_9
action_64 (34) = happyShift action_10
action_64 (35) = happyShift action_11
action_64 (36) = happyShift action_12
action_64 (37) = happyShift action_13
action_64 (38) = happyShift action_14
action_64 (39) = happyShift action_15
action_64 (40) = happyShift action_16
action_64 (41) = happyShift action_17
action_64 (42) = happyShift action_18
action_64 (46) = happyShift action_19
action_64 (5) = happyGoto action_75
action_64 (6) = happyGoto action_3
action_64 (7) = happyGoto action_4
action_64 (10) = happyGoto action_76
action_64 (11) = happyGoto action_77
action_64 (12) = happyGoto action_6
action_64 _ = happyReduce_16

action_65 (30) = happyShift action_7
action_65 (34) = happyShift action_10
action_65 (35) = happyShift action_11
action_65 (38) = happyShift action_14
action_65 (39) = happyShift action_15
action_65 (40) = happyShift action_25
action_65 (41) = happyShift action_17
action_65 (42) = happyShift action_18
action_65 (46) = happyShift action_19
action_65 (11) = happyGoto action_74
action_65 (12) = happyGoto action_6
action_65 _ = happyFail

action_66 _ = happyReduce_4

action_67 (43) = happyShift action_73
action_67 _ = happyFail

action_68 (15) = happyShift action_34
action_68 (16) = happyShift action_35
action_68 (17) = happyShift action_36
action_68 (18) = happyShift action_37
action_68 (19) = happyShift action_38
action_68 (20) = happyShift action_39
action_68 (21) = happyShift action_40
action_68 (22) = happyShift action_41
action_68 (23) = happyShift action_42
action_68 (24) = happyShift action_43
action_68 (27) = happyShift action_44
action_68 (42) = happyShift action_45
action_68 (46) = happyShift action_46
action_68 _ = happyReduce_7

action_69 _ = happyReduce_25

action_70 _ = happyReduce_40

action_71 (30) = happyShift action_7
action_71 (34) = happyShift action_10
action_71 (35) = happyShift action_11
action_71 (38) = happyShift action_14
action_71 (39) = happyShift action_15
action_71 (40) = happyShift action_25
action_71 (41) = happyShift action_17
action_71 (42) = happyShift action_18
action_71 (46) = happyShift action_19
action_71 (11) = happyGoto action_23
action_71 (12) = happyGoto action_6
action_71 (13) = happyGoto action_72
action_71 _ = happyReduce_41

action_72 _ = happyReduce_43

action_73 _ = happyReduce_28

action_74 (15) = happyShift action_34
action_74 (16) = happyShift action_35
action_74 (17) = happyShift action_36
action_74 (18) = happyShift action_37
action_74 (19) = happyShift action_38
action_74 (20) = happyShift action_39
action_74 (21) = happyShift action_40
action_74 (22) = happyShift action_41
action_74 (23) = happyShift action_42
action_74 (24) = happyShift action_43
action_74 (27) = happyShift action_44
action_74 (42) = happyShift action_45
action_74 (46) = happyShift action_46
action_74 _ = happyReduce_6

action_75 (30) = happyShift action_7
action_75 (31) = happyShift action_8
action_75 (32) = happyShift action_9
action_75 (34) = happyShift action_10
action_75 (35) = happyShift action_11
action_75 (36) = happyShift action_12
action_75 (37) = happyShift action_13
action_75 (38) = happyShift action_14
action_75 (39) = happyShift action_15
action_75 (40) = happyShift action_16
action_75 (41) = happyShift action_17
action_75 (42) = happyShift action_18
action_75 (46) = happyShift action_19
action_75 (5) = happyGoto action_75
action_75 (6) = happyGoto action_3
action_75 (7) = happyGoto action_4
action_75 (10) = happyGoto action_90
action_75 (11) = happyGoto action_5
action_75 (12) = happyGoto action_6
action_75 _ = happyReduce_16

action_76 (45) = happyShift action_89
action_76 _ = happyFail

action_77 (15) = happyShift action_34
action_77 (16) = happyShift action_35
action_77 (17) = happyShift action_36
action_77 (18) = happyShift action_37
action_77 (19) = happyShift action_38
action_77 (20) = happyShift action_39
action_77 (21) = happyShift action_40
action_77 (22) = happyShift action_41
action_77 (23) = happyShift action_42
action_77 (24) = happyShift action_43
action_77 (27) = happyShift action_44
action_77 (42) = happyShift action_45
action_77 (45) = happyShift action_88
action_77 (46) = happyShift action_46
action_77 _ = happyReduce_9

action_78 _ = happyReduce_10

action_79 (31) = happyShift action_8
action_79 (44) = happyShift action_64
action_79 (7) = happyGoto action_86
action_79 (9) = happyGoto action_87
action_79 _ = happyFail

action_80 (40) = happyShift action_62
action_80 (14) = happyGoto action_85
action_80 _ = happyReduce_44

action_81 (44) = happyShift action_64
action_81 (9) = happyGoto action_84
action_81 _ = happyFail

action_82 _ = happyReduce_27

action_83 _ = happyReduce_26

action_84 _ = happyReduce_21

action_85 _ = happyReduce_46

action_86 _ = happyReduce_13

action_87 _ = happyReduce_12

action_88 _ = happyReduce_15

action_89 _ = happyReduce_14

action_90 _ = happyReduce_17

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
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Call  (L.tokPosn happy_var_2) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 11 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Call  (L.tokPosn happy_var_2) (S.Ident (L.tokPosn happy_var_1) "print") happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  11 happyReduction_29
happyReduction_29 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Plus happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  11 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Minus happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  11 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Times happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  11 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Divide happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  11 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.Mod happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  11 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.LThan happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  11 happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.GThan happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  11 happyReduction_36
happyReduction_36 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.LTEq happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  11 happyReduction_37
happyReduction_37 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.GTEq happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  11 happyReduction_38
happyReduction_38 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.EqEq happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  11 happyReduction_39
happyReduction_39 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix (L.tokPosn happy_var_2) S.OrOr happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  12 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (S.Array (L.tokPosn happy_var_1) happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  13 happyReduction_41
happyReduction_41  =  HappyAbsSyn13
		 ([]
	)

happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  13 happyReduction_43
happyReduction_43 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_0  14 happyReduction_44
happyReduction_44  =  HappyAbsSyn14
		 ([]
	)

happyReduce_45 = happySpecReduce_1  14 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (let (L.Ident p s) = happy_var_1 in [s]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  14 happyReduction_46
happyReduction_46 (HappyAbsSyn14  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (let (L.Ident p s) = happy_var_1 in s : happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

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
