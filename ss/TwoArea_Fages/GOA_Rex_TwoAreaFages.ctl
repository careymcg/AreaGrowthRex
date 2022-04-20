#V3.30.08.03-trans;_2017_09_29;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_data_and_control_files: GOA_Rex_38_2017.dat // control.ss
#V3.30.08.03-trans;_2017_09_29;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
2  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
3 # recr_dist_method for parameters:  2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
2 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
 2 1 2 0
#
0 #_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
1 #_Nblock_Patterns
1 #_blocks_per_pattern 
# begin and end years of blocks
 1981 1981
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#  autogen
0 0 0 0 0 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
# 
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
2 #_Age(post-settlement)_for_L1;linear growth below this
20 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0  #_placeholder for future growth feature
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
2 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
3 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms																
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn		
	0.1	0.3	0.17	0.17	-1	0	-3	0	0	0	0	0	0	0	#	NatM_p_1_Fem_GP_1
	0.001	30	10	10	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amin_Fem_GP_1
	25	80	45.4584	45.4584	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amax_Fem_GP_1
	0.001	4	0.26553	0.26553	-1	0	1	0	0	0	0	0	0	0	#	VonBert_K_Fem_GP_1
	0.001	0.3	0.170373	0.170373	-1	0	1	0	0	0	0	0	0	0	#	CV_young_Fem_GP_1
	0.001	0.3	0.115224	0.115224	-1	0	1	0	0	0	0	0	0	0	#	CV_old_Fem_GP_1
	0	0.5	1.35E-06	1.35E-06	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Fem
	2.5	4.5	3.44963	3.44963	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Fem
	0	20	5.7159	5.7159	-1	0	-3	0	0	0	0	0	0	0	#	Mat50%_Fem
	-20	3	-1.11252	-1.11252	-1	0	-3	0	0	0	0	0	0	0	#	Mat_slope_Fem
	-3	3	1	1	-1	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_inter_Fem
	-3	4	0	0	-1	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_slope_wt_Fem
	0.1	0.3	0.17	0.17	-1	0	-3	0	0	0	0	0	0	0	#	NatM_p_1_Fem_GP_2
	0.001	30	10	10	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amin_Fem_GP_2
	25	80	44.78714621	44.78714621	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amax_Fem_GP_2
	0.001	4	0.315	0.315	-1	0	1	0	0	0	0	0	0	0	#	VonBert_K_Fem_GP_2
	0.001	0.3	0.13	0.13	-1	0	1	0	0	0	0	0	0	0	#	CV_young_Fem_GP_2
	0.001	0.3	0.08	0.08	-1	0	1	0	0	0	0	0	0	0	#	CV_old_Fem_GP_2
	0	0.5	1.35E-06	1.35E-06	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Fem
	2.5	4.5	3.44963	3.44963	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Fem
	0	20	5.7159	5.7159	-1	0	-3	0	0	0	0	0	0	0	#	Mat50%_Fem
	-20	3	-1.11252	-1.11252	-1	0	-3	0	0	0	0	0	0	0	#	Mat_slope_Fem
	-3	3	1	1	-1	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_inter_Fem
	-3	4	0	0	-1	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_slope_wt_Fem
	0.1	0.3	0.17	0.17	-1	0	-3	0	0	0	0	0	0	0	#	NatM_p_1_Mal_GP_1
	0.001	45	10	10	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amin_Mal_GP_1
	20	70	40.0477	40.0477	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amax_Mal_GP_1
	0.001	4	0.333415	0.333415	-1	0	1	0	0	0	0	0	0	0	#	VonBert_K_Mal_GP_1
	0.001	0.3	0.173568	0.173568	-1	0	1	0	0	0	0	0	0	0	#	CV_young_Mal_GP_1
	0.001	0.3	0.0973328	0.0973328	-1	0	1	0	0	0	0	0	0	0	#	CV_old_Mal_GP_1
	0	0.5	2.18E-06	2.18E-06	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Mal
	2.5	4.5	3.30571	3.30571	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Mal
	0.1	0.3	0.17	0.17	-1	0	-3	0	0	0	0	0	0	0	#	NatM_p_1_Mal_GP_2
	0.001	45	10	10	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amin_Mal_GP_2
	20	70	39.47331088	39.47331088	-1	0	1	0	0	0	0	0	0	0	#	L_at_Amax_Mal_GP_2
	0.001	4	0.379	0.379	-1	0	1	0	0	0	0	0	0	0	#	VonBert_K_Mal_GP_2
	0.001	0.3	0.13	0.13	-1	0	1	0	0	0	0	0	0	0	#	CV_young_Mal_GP_2
	0.001	0.3	0.08	0.08	-1	0	1	0	0	0	0	0	0	0	#	CV_old_Mal_GP_2
	0	0.5	2.18E-06	2.18E-06	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Mal
	2.5	4.5	3.30571	3.30571	-1	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Mal
	-4	4	0	0	-1	0	-1	0	0	0	0	0	0	0	#	RecrDist_GP_1_area_1
	-4	4	0.8	0.8	-1	0	1	0	0	0	0	0	0	0	#	RecrDist_GP_2_area_2
	1	1	1	1	-1	0	-1	0	0	0	0	0	0	0	#	CohortGrowDev
	0.000001	0.999999	0.5	0.5	-1	0	-99	0	0	0	0	0	0	0	#	FracFemale_GP_1
	0.000001	0.999999	0.5	0.5	-1	0	-99	0	0	0	0	0	0	0	#	FracFemale_GP_2

#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
           6.5          13.5       11.0925       11.2822            10             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
           0.2             1             1         0.999          0.05             0         -4          0          0          0          0          0          0          0 # SR_BH_steep
             0             2           0.6           0.6           0.8             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             1             0         -4          0          0          0          0          0          0          0 # SR_regime
             0             0             0             0             0             0        -99          0          0          0          0          0          0          0 # SR_autocorr
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1982 # first year of main recr_devs; early devs can preceed this era
2019 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
-17 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 6 #_recdev_early_phase
 -10 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1958.7 #_last_early_yr_nobias_adj_in_MPD
 1988.5 #_first_yr_fullbias_adj_in_MPD
 2016 #_last_yr_fullbias_adj_in_MPD
 2018.6 #_first_recent_yr_nobias_adj_in_MPD
 0.8599 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -15 #min rec_dev
 15 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1965E 1966E 1967E 1968E 1969E 1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978E 1979E 1980E 1981E 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015F 2016F 2017F 2018F
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0
#
#Fishing Mortality info 
0 # F ballpark
-1984 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
4  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#2018 2038
# F rates by fleet
# Yr:  1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 0.0166782 0.0105251 0.00652841 0.00277278 0.00168117 0.021065 0.0222804 0.011365 0.0244299 0.0938945 0.0648092 0.0677889 0.0863649 0.10084 0.16166 0.0957781 0.0789833 0.0913569 0.109415 0.0910864 0.093941 0.110334 0.0457513 0.066011 0.0996325 0.0869595 0.0826032 0.149054 0.118461 0.0946458 0.0794857 0.120724 0.11859 0.0642702 0.0553752 0.0347309 0.0347309
#
#_Q_setup for every fleet, even if no survey
#_1:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm)
#_2:  extra input for link, i.e. mirror fleet
#_3:  0/1 to select extra sd parameter
#_4:  0/1 for biasadj or not
#_5:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         0         0         0  #  NonEasternSurvey
         3         2         2         1         1         0  #  EasternSurvey
-9999 0 0 0 0 0
##_Q_parms(if_any);Qunits_are_ln(q)
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name
	-1	1	0	0.171799373	0.145066767	6	2	0	0	0	0	0	0	0	#	LnQ_base_NonEasternSurvey(2)
	-1	1	0	0	1	0	-3	0	0	0	0	0	0	0	#	LnQ_base_EasternSurvey(3)
	-1	1	0	0	0.5	0	-2	0	0	0	0	0	0	0	#	Q_extraSD_EasternSurvey(3)

##_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
#            -1             1             0             0           0.5             0         -2          0          0          0          0          0          0          0  #  LnQ_base_NonEasternSurvey(2)
#           -15            15             0             0             1             0          1          0          0          0          0          0          0          0  #  LnQ_base_EasternSurvey(3)
#            -1             1             0             0           0.5             0         -2          0          0          0          0          0          0          0  #  Q_extraSD_EasternSurvey(3)

#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for all sizes
#Pattern:_1; parm=2; logistic; with 95% width specification
#Pattern:_5; parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6; parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8; parm=8; New doublelogistic with smooth transitions and constant above Linf option
#Pattern:_9; parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 0 0 0 0 # 1 Fishery
 0 0 0 0 # 2 NonEasternSurvey
 0 0 0 0 # 3 EasternSurvey
#
#_age_selex_types
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+nages+1; // cubic spline; with 2 additional param for scaling (average over bin range)
#_Pattern Discard Male Special
 20 0 3 0 # 1 Fishery
 20 0 3 0 # 2 NonEasternSurvey
 20 0 3 0 # 3 EasternSurvey
#
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name
	1	20	8.18525	15	5	0	1	0	0	0	0	0	0	0	#	AgeSel_P1_Fishery(1)
	-5	50	30	30	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_P2_Fishery(1)
	-4	12	1.44118	3.21888	5	0	2	0	0	0	0	0	0	0	#	AgeSel_P3_Fishery(1)
	-2	15	8	3.4012	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_P4_Fishery(1)
	-1000	5	-999	-999	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_P5_Fishery(1)
	-5	5	999	999	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_P6_Fishery(1)
	-15	15	-0.814655	0	5	0	3	0	0	0	0	0	0	0	#	AgeSel_1Male_Peak_Fishery
	-15	15	-0.0762122	0	5	0	3	0	0	0	0	0	0	0	#	AgeSel_1Male_Ascend_Fishery
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_1Male_Descend_Fishery
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_1Male_Final_Fishery
	-15	15	1	0	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_1Male_Scale_Fishery
	1	20	5.51323	11	5	0	1	0	0	0	0	0	0	0	#	AgeSel_P1_NonEasternSurvey(2)
	-5	50	30	30	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_P2_NonEasternSurvey(2)
	-4	12	1.4424	4.09434	5	0	2	0	0	0	0	0	0	0	#	AgeSel_P3_NonEasternSurvey(2)
	-2	15	8	3.4012	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_P4_NonEasternSurvey(2)
	-1000	5	-999	-999	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_P5_NonEasternSurvey(2)
	-5	5	999	999	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_P6_NonEasternSurvey(2)
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_2Male_Peak_NonEasternSurvey
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_2Male_Ascend_NonEasternSurvey
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_2Male_Descend_NonEasternSurvey
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_2Male_Final_NonEasternSurvey
	-15	15	1	0	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_2Male_Scale_NonEasternSurvey
	1	20	7.87807	11	5	0	1	0	0	0	0	0	0	0	#	AgeSel_P1_EasternSurvey(3)
	-5	50	30	30	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_P2_EasternSurvey(3)
	-4	12	2.32766	4.09434	5	0	2	0	0	0	0	0	0	0	#	AgeSel_P3_EasternSurvey(3)
	-2	15	8	3.4012	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_P4_EasternSurvey(3)
	-1000	5	-999	-999	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_P5_EasternSurvey(3)
	-5	5	999	999	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_P6_EasternSurvey(3)
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_3Male_Peak_EasternSurvey
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_3Male_Ascend_EasternSurvey
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_3Male_Descend_EasternSurvey
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#	AgeSel_3Male_Final_EasternSurvey
	-15	15	1	0	5	0	-4	0	0	0	0	0	0	0	#	AgeSel_3Male_Scale_EasternSurvey
#_no timevary selex parameters
#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#Factor Fleet New_Var_adj hash Old_Var_adj New_Francis New_MI Francis_mult Francis_lo Francis_hi MI_mult Type Name Note
4 1 0.03 # 0.03 0.03 0.27 1.15 0.67 3.1 8.86 len Fishery 
4 2 0.79 # 0.61 0.79 1.34 1.29 0.83 2.96 2.19 len NonEasternSurvey 
4 3 0.4 # 0.38 0.4 2.84 1.07 0.76 2.65 7.48 len EasternSurvey 
5 1 0.32 # 0.29 0.32 0.64 1.1 0.64 3.36 2.19 age Fishery 
5 2 0.08 # 0.09 0.08 0.43 0.89 0.59 2.11 4.82 age NonEasternSurvey 
5 3 0.1 # 0.12 0.1 0.56 0.86 0.6 2.18 4.65 age EasternSurvey 

 -9999   1    0  # terminator
#
10 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 0 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 0 0 0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 1 1 1 1 1 1 #_CPUE/survey:_2
#  1 1 1 1 1 1 1 1 1 1 #_CPUE/survey:_3
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_1
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_2
#  1 1 1 1 1 1 1 1 1 1 #_lencomp:_3
#  1 1 1 1 1 1 1 1 1 1 #_agecomp:_1
#  1 1 1 1 1 1 1 1 1 1 #_agecomp:_2
#  1 1 1 1 1 1 1 1 1 1 #_agecomp:_3
#  1 1 1 1 1 1 1 1 1 1 #_init_equ_catch
#  1 1 1 1 1 1 1 1 1 1 #_recruitments
#  1 1 1 1 1 1 1 1 1 1 #_parameter-priors
#  1 1 1 1 1 1 1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 1 1 1 1 1 1 #_crashPenLambda
#  0 0 0 0 0 0 0 0 0 0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

