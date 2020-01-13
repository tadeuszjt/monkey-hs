{-# OPTIONS_GHC -w #-}
module Parser where
import qualified Lexer as L
import qualified AST as S
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
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

action_0 (29) = happyShift action_6
action_0 (30) = happyShift action_7
action_0 (32) = happyShift action_8
action_0 (33) = happyShift action_9
action_0 (34) = happyShift action_10
action_0 (35) = happyShift action_11
action_0 (36) = happyShift action_12
action_0 (37) = happyShift action_13
action_0 (38) = happyShift action_14
action_0 (39) = happyShift action_15
action_0 (4) = happyGoto action_16
action_0 (5) = happyGoto action_17
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 _ = happyFail

action_1 (29) = happyShift action_6
action_1 (30) = happyShift action_7
action_1 (32) = happyShift action_8
action_1 (33) = happyShift action_9
action_1 (34) = happyShift action_10
action_1 (35) = happyShift action_11
action_1 (36) = happyShift action_12
action_1 (37) = happyShift action_13
action_1 (38) = happyShift action_14
action_1 (39) = happyShift action_15
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (44) = happyShift action_39
action_3 _ = happyFail

action_4 _ = happyReduce_5

action_5 (14) = happyShift action_27
action_5 (15) = happyShift action_28
action_5 (16) = happyShift action_29
action_5 (17) = happyShift action_30
action_5 (18) = happyShift action_31
action_5 (19) = happyShift action_32
action_5 (20) = happyShift action_33
action_5 (21) = happyShift action_34
action_5 (22) = happyShift action_35
action_5 (23) = happyShift action_36
action_5 (26) = happyShift action_37
action_5 (39) = happyShift action_38
action_5 _ = happyReduce_9

action_6 (39) = happyShift action_26
action_6 _ = happyFail

action_7 (29) = happyShift action_6
action_7 (32) = happyShift action_8
action_7 (33) = happyShift action_9
action_7 (36) = happyShift action_12
action_7 (37) = happyShift action_20
action_7 (38) = happyShift action_14
action_7 (39) = happyShift action_15
action_7 (11) = happyGoto action_25
action_7 _ = happyFail

action_8 _ = happyReduce_18

action_9 _ = happyReduce_19

action_10 (29) = happyShift action_6
action_10 (32) = happyShift action_8
action_10 (33) = happyShift action_9
action_10 (36) = happyShift action_12
action_10 (37) = happyShift action_20
action_10 (38) = happyShift action_14
action_10 (39) = happyShift action_15
action_10 (11) = happyGoto action_24
action_10 _ = happyFail

action_11 (29) = happyShift action_6
action_11 (32) = happyShift action_8
action_11 (33) = happyShift action_9
action_11 (36) = happyShift action_12
action_11 (37) = happyShift action_20
action_11 (38) = happyShift action_14
action_11 (39) = happyShift action_15
action_11 (11) = happyGoto action_23
action_11 _ = happyFail

action_12 _ = happyReduce_17

action_13 (27) = happyShift action_21
action_13 (28) = happyShift action_22
action_13 _ = happyReduce_22

action_14 _ = happyReduce_21

action_15 (29) = happyShift action_6
action_15 (32) = happyShift action_8
action_15 (33) = happyShift action_9
action_15 (36) = happyShift action_12
action_15 (37) = happyShift action_20
action_15 (38) = happyShift action_14
action_15 (39) = happyShift action_15
action_15 (11) = happyGoto action_19
action_15 _ = happyFail

action_16 (45) = happyAccept
action_16 _ = happyFail

action_17 (29) = happyShift action_6
action_17 (30) = happyShift action_7
action_17 (32) = happyShift action_8
action_17 (33) = happyShift action_9
action_17 (34) = happyShift action_10
action_17 (35) = happyShift action_11
action_17 (36) = happyShift action_12
action_17 (37) = happyShift action_13
action_17 (38) = happyShift action_14
action_17 (39) = happyShift action_15
action_17 (4) = happyGoto action_18
action_17 (5) = happyGoto action_17
action_17 (6) = happyGoto action_3
action_17 (7) = happyGoto action_4
action_17 (11) = happyGoto action_5
action_17 _ = happyReduce_1

action_18 _ = happyReduce_2

action_19 (14) = happyShift action_27
action_19 (15) = happyShift action_28
action_19 (16) = happyShift action_29
action_19 (17) = happyShift action_30
action_19 (18) = happyShift action_31
action_19 (19) = happyShift action_32
action_19 (20) = happyShift action_33
action_19 (21) = happyShift action_34
action_19 (22) = happyShift action_35
action_19 (23) = happyShift action_36
action_19 (26) = happyShift action_37
action_19 (39) = happyShift action_38
action_19 (40) = happyShift action_60
action_19 _ = happyFail

action_20 _ = happyReduce_22

action_21 (29) = happyShift action_6
action_21 (32) = happyShift action_8
action_21 (33) = happyShift action_9
action_21 (36) = happyShift action_12
action_21 (37) = happyShift action_20
action_21 (38) = happyShift action_14
action_21 (39) = happyShift action_15
action_21 (11) = happyGoto action_59
action_21 _ = happyFail

action_22 (29) = happyShift action_6
action_22 (32) = happyShift action_8
action_22 (33) = happyShift action_9
action_22 (36) = happyShift action_12
action_22 (37) = happyShift action_20
action_22 (38) = happyShift action_14
action_22 (39) = happyShift action_15
action_22 (11) = happyGoto action_58
action_22 _ = happyFail

action_23 (14) = happyShift action_27
action_23 (15) = happyShift action_28
action_23 (16) = happyShift action_29
action_23 (17) = happyShift action_30
action_23 (18) = happyShift action_31
action_23 (19) = happyShift action_32
action_23 (20) = happyShift action_33
action_23 (21) = happyShift action_34
action_23 (22) = happyShift action_35
action_23 (23) = happyShift action_36
action_23 (26) = happyShift action_37
action_23 (39) = happyShift action_38
action_23 _ = happyReduce_8

action_24 (14) = happyShift action_27
action_24 (15) = happyShift action_28
action_24 (16) = happyShift action_29
action_24 (17) = happyShift action_30
action_24 (18) = happyShift action_31
action_24 (19) = happyShift action_32
action_24 (20) = happyShift action_33
action_24 (21) = happyShift action_34
action_24 (22) = happyShift action_35
action_24 (23) = happyShift action_36
action_24 (26) = happyShift action_37
action_24 (39) = happyShift action_38
action_24 (41) = happyShift action_56
action_24 (9) = happyGoto action_57
action_24 _ = happyFail

action_25 (14) = happyShift action_27
action_25 (15) = happyShift action_28
action_25 (16) = happyShift action_29
action_25 (17) = happyShift action_30
action_25 (18) = happyShift action_31
action_25 (19) = happyShift action_32
action_25 (20) = happyShift action_33
action_25 (21) = happyShift action_34
action_25 (22) = happyShift action_35
action_25 (23) = happyShift action_36
action_25 (26) = happyShift action_37
action_25 (39) = happyShift action_38
action_25 (41) = happyShift action_56
action_25 (9) = happyGoto action_55
action_25 _ = happyFail

action_26 (37) = happyShift action_54
action_26 (13) = happyGoto action_53
action_26 _ = happyReduce_39

action_27 (29) = happyShift action_6
action_27 (32) = happyShift action_8
action_27 (33) = happyShift action_9
action_27 (36) = happyShift action_12
action_27 (37) = happyShift action_20
action_27 (38) = happyShift action_14
action_27 (39) = happyShift action_15
action_27 (11) = happyGoto action_52
action_27 _ = happyFail

action_28 (29) = happyShift action_6
action_28 (32) = happyShift action_8
action_28 (33) = happyShift action_9
action_28 (36) = happyShift action_12
action_28 (37) = happyShift action_20
action_28 (38) = happyShift action_14
action_28 (39) = happyShift action_15
action_28 (11) = happyGoto action_51
action_28 _ = happyFail

action_29 (29) = happyShift action_6
action_29 (32) = happyShift action_8
action_29 (33) = happyShift action_9
action_29 (36) = happyShift action_12
action_29 (37) = happyShift action_20
action_29 (38) = happyShift action_14
action_29 (39) = happyShift action_15
action_29 (11) = happyGoto action_50
action_29 _ = happyFail

action_30 (29) = happyShift action_6
action_30 (32) = happyShift action_8
action_30 (33) = happyShift action_9
action_30 (36) = happyShift action_12
action_30 (37) = happyShift action_20
action_30 (38) = happyShift action_14
action_30 (39) = happyShift action_15
action_30 (11) = happyGoto action_49
action_30 _ = happyFail

action_31 (29) = happyShift action_6
action_31 (32) = happyShift action_8
action_31 (33) = happyShift action_9
action_31 (36) = happyShift action_12
action_31 (37) = happyShift action_20
action_31 (38) = happyShift action_14
action_31 (39) = happyShift action_15
action_31 (11) = happyGoto action_48
action_31 _ = happyFail

action_32 (29) = happyShift action_6
action_32 (32) = happyShift action_8
action_32 (33) = happyShift action_9
action_32 (36) = happyShift action_12
action_32 (37) = happyShift action_20
action_32 (38) = happyShift action_14
action_32 (39) = happyShift action_15
action_32 (11) = happyGoto action_47
action_32 _ = happyFail

action_33 (29) = happyShift action_6
action_33 (32) = happyShift action_8
action_33 (33) = happyShift action_9
action_33 (36) = happyShift action_12
action_33 (37) = happyShift action_20
action_33 (38) = happyShift action_14
action_33 (39) = happyShift action_15
action_33 (11) = happyGoto action_46
action_33 _ = happyFail

action_34 (29) = happyShift action_6
action_34 (32) = happyShift action_8
action_34 (33) = happyShift action_9
action_34 (36) = happyShift action_12
action_34 (37) = happyShift action_20
action_34 (38) = happyShift action_14
action_34 (39) = happyShift action_15
action_34 (11) = happyGoto action_45
action_34 _ = happyFail

action_35 (29) = happyShift action_6
action_35 (32) = happyShift action_8
action_35 (33) = happyShift action_9
action_35 (36) = happyShift action_12
action_35 (37) = happyShift action_20
action_35 (38) = happyShift action_14
action_35 (39) = happyShift action_15
action_35 (11) = happyGoto action_44
action_35 _ = happyFail

action_36 (29) = happyShift action_6
action_36 (32) = happyShift action_8
action_36 (33) = happyShift action_9
action_36 (36) = happyShift action_12
action_36 (37) = happyShift action_20
action_36 (38) = happyShift action_14
action_36 (39) = happyShift action_15
action_36 (11) = happyGoto action_43
action_36 _ = happyFail

action_37 (29) = happyShift action_6
action_37 (32) = happyShift action_8
action_37 (33) = happyShift action_9
action_37 (36) = happyShift action_12
action_37 (37) = happyShift action_20
action_37 (38) = happyShift action_14
action_37 (39) = happyShift action_15
action_37 (11) = happyGoto action_42
action_37 _ = happyFail

action_38 (29) = happyShift action_6
action_38 (32) = happyShift action_8
action_38 (33) = happyShift action_9
action_38 (36) = happyShift action_12
action_38 (37) = happyShift action_20
action_38 (38) = happyShift action_14
action_38 (39) = happyShift action_15
action_38 (11) = happyGoto action_40
action_38 (12) = happyGoto action_41
action_38 _ = happyReduce_36

action_39 _ = happyReduce_3

action_40 (14) = happyShift action_27
action_40 (15) = happyShift action_28
action_40 (16) = happyShift action_29
action_40 (17) = happyShift action_30
action_40 (18) = happyShift action_31
action_40 (19) = happyShift action_32
action_40 (20) = happyShift action_33
action_40 (21) = happyShift action_34
action_40 (22) = happyShift action_35
action_40 (23) = happyShift action_36
action_40 (26) = happyShift action_37
action_40 (39) = happyShift action_38
action_40 (43) = happyShift action_68
action_40 _ = happyReduce_37

action_41 (40) = happyShift action_67
action_41 _ = happyFail

action_42 (14) = happyShift action_27
action_42 (15) = happyShift action_28
action_42 (16) = happyShift action_29
action_42 (17) = happyShift action_30
action_42 (18) = happyShift action_31
action_42 (19) = happyShift action_32
action_42 (20) = happyShift action_33
action_42 (21) = happyShift action_34
action_42 (22) = happyShift action_35
action_42 (23) = happyShift action_36
action_42 (39) = happyShift action_38
action_42 _ = happyReduce_35

action_43 (14) = happyShift action_27
action_43 (15) = happyShift action_28
action_43 (16) = happyShift action_29
action_43 (17) = happyShift action_30
action_43 (18) = happyShift action_31
action_43 (19) = happyShift action_32
action_43 (20) = happyShift action_33
action_43 (21) = happyShift action_34
action_43 (22) = happyShift action_35
action_43 (39) = happyShift action_38
action_43 _ = happyReduce_34

action_44 (21) = happyFail
action_44 (22) = happyFail
action_44 (39) = happyShift action_38
action_44 _ = happyReduce_33

action_45 (21) = happyFail
action_45 (22) = happyFail
action_45 (39) = happyShift action_38
action_45 _ = happyReduce_32

action_46 (19) = happyFail
action_46 (20) = happyFail
action_46 (21) = happyShift action_34
action_46 (22) = happyShift action_35
action_46 (39) = happyShift action_38
action_46 _ = happyReduce_31

action_47 (19) = happyFail
action_47 (20) = happyFail
action_47 (21) = happyShift action_34
action_47 (22) = happyShift action_35
action_47 (39) = happyShift action_38
action_47 _ = happyReduce_30

action_48 (14) = happyShift action_27
action_48 (15) = happyShift action_28
action_48 (16) = happyShift action_29
action_48 (19) = happyShift action_32
action_48 (20) = happyShift action_33
action_48 (21) = happyShift action_34
action_48 (22) = happyShift action_35
action_48 (39) = happyShift action_38
action_48 _ = happyReduce_26

action_49 (14) = happyShift action_27
action_49 (15) = happyShift action_28
action_49 (16) = happyShift action_29
action_49 (19) = happyShift action_32
action_49 (20) = happyShift action_33
action_49 (21) = happyShift action_34
action_49 (22) = happyShift action_35
action_49 (39) = happyShift action_38
action_49 _ = happyReduce_25

action_50 (19) = happyShift action_32
action_50 (20) = happyShift action_33
action_50 (21) = happyShift action_34
action_50 (22) = happyShift action_35
action_50 (39) = happyShift action_38
action_50 _ = happyReduce_29

action_51 (19) = happyShift action_32
action_51 (20) = happyShift action_33
action_51 (21) = happyShift action_34
action_51 (22) = happyShift action_35
action_51 (39) = happyShift action_38
action_51 _ = happyReduce_28

action_52 (19) = happyShift action_32
action_52 (20) = happyShift action_33
action_52 (21) = happyShift action_34
action_52 (22) = happyShift action_35
action_52 (39) = happyShift action_38
action_52 _ = happyReduce_27

action_53 (40) = happyShift action_66
action_53 _ = happyFail

action_54 (43) = happyShift action_65
action_54 _ = happyFail

action_55 (31) = happyShift action_64
action_55 (8) = happyGoto action_63
action_55 _ = happyReduce_11

action_56 (29) = happyShift action_6
action_56 (30) = happyShift action_7
action_56 (32) = happyShift action_8
action_56 (33) = happyShift action_9
action_56 (34) = happyShift action_10
action_56 (35) = happyShift action_11
action_56 (36) = happyShift action_12
action_56 (37) = happyShift action_13
action_56 (38) = happyShift action_14
action_56 (39) = happyShift action_15
action_56 (5) = happyGoto action_61
action_56 (6) = happyGoto action_3
action_56 (7) = happyGoto action_4
action_56 (10) = happyGoto action_62
action_56 (11) = happyGoto action_5
action_56 _ = happyReduce_15

action_57 _ = happyReduce_4

action_58 (14) = happyShift action_27
action_58 (15) = happyShift action_28
action_58 (16) = happyShift action_29
action_58 (17) = happyShift action_30
action_58 (18) = happyShift action_31
action_58 (19) = happyShift action_32
action_58 (20) = happyShift action_33
action_58 (21) = happyShift action_34
action_58 (22) = happyShift action_35
action_58 (23) = happyShift action_36
action_58 (26) = happyShift action_37
action_58 (39) = happyShift action_38
action_58 _ = happyReduce_7

action_59 (14) = happyShift action_27
action_59 (15) = happyShift action_28
action_59 (16) = happyShift action_29
action_59 (17) = happyShift action_30
action_59 (18) = happyShift action_31
action_59 (19) = happyShift action_32
action_59 (20) = happyShift action_33
action_59 (21) = happyShift action_34
action_59 (22) = happyShift action_35
action_59 (23) = happyShift action_36
action_59 (26) = happyShift action_37
action_59 (39) = happyShift action_38
action_59 _ = happyReduce_6

action_60 _ = happyReduce_23

action_61 (29) = happyShift action_6
action_61 (30) = happyShift action_7
action_61 (32) = happyShift action_8
action_61 (33) = happyShift action_9
action_61 (34) = happyShift action_10
action_61 (35) = happyShift action_11
action_61 (36) = happyShift action_12
action_61 (37) = happyShift action_13
action_61 (38) = happyShift action_14
action_61 (39) = happyShift action_15
action_61 (5) = happyGoto action_61
action_61 (6) = happyGoto action_3
action_61 (7) = happyGoto action_4
action_61 (10) = happyGoto action_75
action_61 (11) = happyGoto action_5
action_61 _ = happyReduce_15

action_62 (42) = happyShift action_74
action_62 _ = happyFail

action_63 _ = happyReduce_10

action_64 (30) = happyShift action_7
action_64 (41) = happyShift action_56
action_64 (7) = happyGoto action_72
action_64 (9) = happyGoto action_73
action_64 _ = happyFail

action_65 (37) = happyShift action_54
action_65 (13) = happyGoto action_71
action_65 _ = happyReduce_39

action_66 (41) = happyShift action_56
action_66 (9) = happyGoto action_70
action_66 _ = happyFail

action_67 _ = happyReduce_24

action_68 (29) = happyShift action_6
action_68 (32) = happyShift action_8
action_68 (33) = happyShift action_9
action_68 (36) = happyShift action_12
action_68 (37) = happyShift action_20
action_68 (38) = happyShift action_14
action_68 (39) = happyShift action_15
action_68 (11) = happyGoto action_40
action_68 (12) = happyGoto action_69
action_68 _ = happyReduce_36

action_69 _ = happyReduce_38

action_70 _ = happyReduce_20

action_71 _ = happyReduce_40

action_72 _ = happyReduce_13

action_73 _ = happyReduce_12

action_74 _ = happyReduce_14

action_75 _ = happyReduce_16

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
	_
	 =  HappyAbsSyn5
		 (S.While happy_var_2 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (L.Ident _ happy_var_1))
	 =  HappyAbsSyn6
		 (S.Assign happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (L.Ident _ happy_var_1))
	 =  HappyAbsSyn6
		 (S.Set happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (S.Return happy_var_2
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
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (S.If happy_var_2 happy_var_3 happy_var_4
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
	_
	 =  HappyAbsSyn9
		 (S.Block happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  10 happyReduction_15
happyReduction_15  =  HappyAbsSyn10
		 ([]
	)

happyReduce_16 = happySpecReduce_2  10 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (L.Int _ happy_var_1))
	 =  HappyAbsSyn11
		 (S.Int happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn11
		 (S.Bool True
	)

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn11
		 (S.Bool False
	)

happyReduce_20 = happyReduce 5 11 happyReduction_20
happyReduction_20 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Func happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyTerminal (L.String _ happy_var_1))
	 =  HappyAbsSyn11
		 (S.String happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 (HappyTerminal (L.Ident _ happy_var_1))
	 =  HappyAbsSyn11
		 (S.Ident happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 11 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (S.Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  11 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.Plus happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.Minus happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.Times happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  11 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.Divide happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  11 happyReduction_29
happyReduction_29 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.Mod happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  11 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.LThan happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  11 happyReduction_31
happyReduction_31 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.GThan happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  11 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.LTEq happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  11 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.GTEq happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  11 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.EqEq happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  11 happyReduction_35
happyReduction_35 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (S.Infix S.OrOr happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_0  12 happyReduction_36
happyReduction_36  =  HappyAbsSyn12
		 ([]
	)

happyReduce_37 = happySpecReduce_1  12 happyReduction_37
happyReduction_37 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  12 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  13 happyReduction_39
happyReduction_39  =  HappyAbsSyn13
		 ([]
	)

happyReduce_40 = happySpecReduce_3  13 happyReduction_40
happyReduction_40 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (L.Ident _ happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 45 45 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.ReservedOp _ "*" -> cont 14;
	L.ReservedOp _ "/" -> cont 15;
	L.ReservedOp _ "%" -> cont 16;
	L.ReservedOp _ "+" -> cont 17;
	L.ReservedOp _ "-" -> cont 18;
	L.ReservedOp _ "<" -> cont 19;
	L.ReservedOp _ ">" -> cont 20;
	L.ReservedOp _ "<=" -> cont 21;
	L.ReservedOp _ ">=" -> cont 22;
	L.ReservedOp _ "==" -> cont 23;
	L.ReservedOp _ "!=" -> cont 24;
	L.ReservedOp _ "&&" -> cont 25;
	L.ReservedOp _ "||" -> cont 26;
	L.ReservedOp _ ":=" -> cont 27;
	L.ReservedOp _ "=" -> cont 28;
	L.Reserved _ "fn" -> cont 29;
	L.Reserved _ "if" -> cont 30;
	L.Reserved _ "else" -> cont 31;
	L.Reserved _ "true" -> cont 32;
	L.Reserved _ "false" -> cont 33;
	L.Reserved _ "while" -> cont 34;
	L.Reserved _ "return" -> cont 35;
	L.Int _ happy_dollar_dollar -> cont 36;
	L.Ident _ happy_dollar_dollar -> cont 37;
	L.String _ happy_dollar_dollar -> cont 38;
	L.Sym _ '(' -> cont 39;
	L.Sym _ ')' -> cont 40;
	L.Sym _ '{' -> cont 41;
	L.Sym _ '}' -> cont 42;
	L.Sym _ ',' -> cont 43;
	L.Sym _ ';' -> cont 44;
	_ -> happyError' (tk:tks)
	}

happyError_ 45 tk tks = happyError' tks
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
