* Read in data;
PROC IMPORT OUT = dataset 
			DATAFILE = '/home/u43389324/Happiness/World_Happiness_Data.xlsx'
            DBMS=xlsx REPLACE; GETNAMES=YES;
RUN;

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DATA CLEANING / EDA ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

* Descriptive statistics;
PROC MEANS data=dataset min q1 median q3 max mean std var NMISS N maxdec=2;
	VAR Happiness GDP Support Healthy Freedom Generosity Corruption;
RUN;


* transpose data to long form;
PROC TRANSPOSE DATA=dataset OUT=dataset_long;
   VAR GDP Support Healthy Freedom Generosity Corruption;
   BY Rank Country CNTRY;
RUN;
DATA dataset_long;
  SET dataset_long (RENAME=(col1=value));
  SET dataset_long (RENAME=(_LABEL_=category));
  DROP COL1;
  DROP _name_;
  DROP _LABEL_;
RUN;   


* Boxplot distributions;
* --- since spread of data is wide, will need to standardize data;
PROC SGPLOT DATA=dataset_long; VBOX value / GROUP=category; KEYLEGEND / TITLE=""; RUN;


* Standardize data;
PROC STDIZE DATA=dataset OUT=dataset_std METHOD=std;
	VAR GDP Support Healthy Freedom Generosity Corruption;
RUN;


* transpose data to long form;
PROC TRANSPOSE DATA=dataset_std OUT=dataset_long;
   VAR GDP Support Healthy Freedom Generosity Corruption;
   BY Rank Country CNTRY;
RUN;
DATA dataset_long;
  SET dataset_long (RENAME=(col1=value));
  SET dataset_long (RENAME=(_LABEL_=category));
  DROP COL1;
  DROP _name_;
RUN; 


* Boxplot distributions;
PROC SGPLOT DATA=dataset_long; VBOX value / GROUP=category; KEYLEGEND / TITLE=""; RUN;


* Correlation matrix;
PROC CORR DATA=dataset_std;
    VAR GDP Support Healthy Freedom Generosity Corruption;
	ods output PearsonCorr=p;
RUN;

* Obtain lower triangle of corr matrix;
* source: https://blogs.sas.com/content/graphicallyspeaking/2017/12/11/displaying-upper-lower-triangle-correlation-matrix/;
DATA p2;
    SET p END=__eof;
    ARRAY __n[*] _NUMERIC_;
    DO __i = _n_ TO dim(__n); __n[__i] = ._; END;
    IF _n_ = 1 THEN DO;
        CALL execute('data _null_; set p2;');
        CALL execute('file print ods=(template="Base.Corr.StackedMatrix"');
    	CALL execute('columns=(rowname=variable');
    END;
	CALL execute(CATS('matrix=',VNAME(__n[_n_]),'(generic)'));
	IF __eof THEN CALL execute(')); put _ods_; run;');
RUN;

* bivariate plots;
PROC SGSCATTER DATA=dataset_std;
	MATRIX GDP Support Healthy Freedom Generosity Corruption / 
	DIAGONAL=(KERNEL HISTOGRAM);
RUN;



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Cluster Analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


* ~~~~~~~~~~~~~~~~~~~~~ Agglomerative Methods ~~~~~~~~~~~~~~~~~~~~~;

/*--- Agglomerative: macro for clustering and crosstabulating ---*/
%MACRO AgglomCluster(ds, clustMethod, OutName);
	PROC CLUSTER DATA=&ds METHOD=&clustMethod RSQ NOSQUARE NOEIGEN NONORM OUT=&OutName; 
		VAR GDP Support Healthy Freedom Generosity Corruption;
		ID CNTRY; 
	PROC SORT; BY _ncl_;

	PROC TREE DATA=&OutName; ID CNTRY;                   * Create dendrograms;
	DATA &OutName; SET &OutName; &clustMethod = _rsq_;   * Adding R sq values for each method to new variable;
	RUN;
%MEND AgglomCluster;


/*---  Loop through each inter-cluster method and run analysis ---*/
%let param1 = single    complete    centroid    ward    average;
%let param2 = singleout completeout centroidout wardout averageout;
%macro loop();
    %do i=1 %to %sysfunc(countw(&param1,%str( )));
        %let thisparam1=%scan(&param1,&i,%str( ));
        %let thisparam2=%scan(&param2,&i,%str( ));
        %put &thisparam1 &thisparam2;
        %AgglomCluster(dataset_std, &thisparam1, &thisparam2);
    %end;
%mend loop;
%loop;
 

*Merging all output values for all methods into one dataset sorted by # Clusters;
DATA outputs; MERGE singleout completeout centroidout wardout averageout; BY _ncl_;
DATA outputs; SET outputs; IF _ncl_ <20;

symbol1 i = join v = S l = 1  c = gray; 
symbol2 i = join v = P l = 5  c = blue;
symbol3 i = join v = C l = 10 c = orange; 
symbol4 i = join v = W l = 15 c = red;
symbol5 i = join v = A l = 20 c = CX00FF00;

PROC GPLOT DATA = outputs;
	PLOT single*_ncl_=1 complete*_ncl_=2 centroid*_ncl_=3 ward*_ncl_=4 average*_ncl_=5 /OVERLAY LEGEND;
RUN;



* Selected method results;
PROC TREE DATA = wardout OUT = clus NCLUSTERS = 5; 
	ID CNTRY; COPY GDP Support Healthy Freedom Generosity Corruption;
RUN;
PROC SORT; BY cluster; RUN;
PROC PRINT; 
	BY cluster; 
	VAR CNTRY GDP Support Healthy Freedom Generosity Corruption;
RUN;	

/* To create a Scatterplot */
PROC CANDISC DATA = clus OUT = can;
	CLASS cluster;
	VAR GDP Support Healthy Freedom Generosity Corruption;
RUN;
PROC SGPLOT DATA = can;
	TITLE "Cluster Analysis for Ward's Error";
	SCATTER Y = can2 X = can1 / GROUP = cluster 
	MARKERATTRS = (SIZE=9 symbol=CircleFilled);
	STYLEATTRS
	DATACONTRASTCOLORS = (cyan blue orange red bilg magenta yellow);
RUN;


/*export data from PROC CANDISC to file called can_km.xlsx*/
PROC EXPORT DATA=can
    OUTFILE="/home/u43389324/Happiness/can_wards.xlsx"
    DBMS=xlsx
    REPLACE;
    SHEET="Sheet 1";
RUN;







* ~~~~~~~~~~~~~~~~~~~~~ Principle Componet Analysis ~~~~~~~~~~~~~~~~~~~~~;
* A PCA is conducted because some of the variables in the dataset are correlated, 
  and they need to be uncorrelated to use the approximate expected R square and 
  the cubic clustering criterion to assess the optimal cluster size;
  
PROC PRINCOMP DATA=dataset_std OUT=dataset_PCA;
	VAR GDP Support Healthy Freedom Generosity Corruption;
RUN;

* Calculate the structual loads to interpret new PC axes;
PROC CORR noprob nosimple;
	VAR  GDP   Support Healthy Freedom Generosity Corruption;
	WITH Prin1 Prin2   Prin3   Prin4   Prin5      Prin6;
RUN;


* The first two PCs are selected because...
  1. They have eigenvalues >= 1
  2. Together they explain 70% of the variance in the data
  3. The scree plot has an elbow at two PCs;
 


* ~~~~~~~~~~~~~~~~~~~~~ K-MEANS ~~~~~~~~~~~~~~~~~~~~~;

/*--- KMEANS: macro for clustering and crosstabulating ---*/
* radius         = Specifies minimum distance for selecting new seeds for inital centroids;
* replace        = indicates how initial cluster centroids are selected (none/full/rand/part) to limit seed replacement;
* max clusters   = Specifies maximum number of clusters;
* max iterations = Specifies maximum number of iterations for recomputing cluster seed (i.e., initial centroids);
* list           = displays cluster assignments for all observations (and dist from final center seed);
* distance       = displays distances between cluster centers;


PROC DELETE DATA=F_stat;  RUN;
PROC DELETE DATA=App_RSQ; RUN;
PROC DELETE DATA=CCC;     RUN;

* Conduct k-means cluster anlaysis using the PC scores for values k = 2 to 15;
%MACRO doFASTCLUS;
	%DO k=2 %TO 20;
		* Collect pseudo F-stat, approx rsq, and CCC values per k value analysis;
		ods output PseudoFStat         =  F_stat&k.(keep = value rename = (value = f_value));
		ods output ApproxExpOverAllRSq = App_RSQ&k.(keep = value rename = (value = aprx_rsq));
		ods output CCC                 =     CCC&k.(keep = value rename = (value = ccc));
	
		*Conduct K-mean;
		PROC FASTCLUS DATA=dataset_PCA OUT=dsKM_&k MAXITER=100  MAXCLUSTERS=&k SUMMARY REPLACE=full;
			VAR Prin1 Prin2;
		RUN;
	
		* Save evaluation stats;
		DATA F_stat&k;  SET F_stat&k.;  cluster=&k.; RUN;
		DATA App_RSQ&k; SET App_RSQ&k.; cluster=&k.; RUN;	
		DATA CCC&k;     SET CCC&k.;     cluster=&k.; RUN;
	
		* Collect stat values per k-value into one dataset;
		PROC APPEND BASE=F_stat  DATA=F_stat&k.;  RUN; PROC DELETE DATA=F_stat&k.; RUN;
		PROC APPEND BASE=App_RSQ DATA=App_RSQ&k.; RUN; PROC DELETE DATA=App_RSQ&k.; RUN;
		PROC APPEND BASE=CCC     DATA=CCC&k.;     RUN; PROC DELETE DATA=CCC&k.; RUN;
	%END;
	
	* Plot Stats;
	PROC SGPLOT DATA=F_stat;
		SERIES Y=f_value x=cluster / MARKERS LINEATTRS = (THICKNESS = 2);
		YAXIS LABEL = "Pseudo F-Statistics";
		XAXIS LABEL = "Cluster Size" VALUES = (2 TO 20 BY 1);
	RUN;
	PROC SGPLOT DATA=App_RSQ;
		SERIES Y=aprx_rsq x=cluster / MARKERS LINEATTRS = (THICKNESS = 2);
		YAXIS LABEL = "Approximate Expected Over-All R-Square";
		XAXIS LABEL = "Cluster Size" VALUES = (2 TO 20 BY 1);
	RUN;
	PROC SGPLOT DATA=CCC;
		SERIES Y=ccc x=cluster / MARKERS LINEATTRS = (THICKNESS = 2);
		YAXIS LABEL = "Cubic Clustering Criterion (CCC)";
		XAXIS LABEL = "Cluster Size" VALUES = (2 TO 20 BY 1);
	RUN;
	
%MEND; 
%doFASTCLUS;


/* To create a Scatterplot */
PROC CANDISC DATA = dsKM_7 OUT = can_km;
	CLASS cluster;
	VAR GDP Support Healthy Freedom Generosity Corruption;
RUN;
PROC SGPLOT DATA = can_km;
	TITLE "K-means Cluster Analysis for K = 7";
	SCATTER Y = can2 X = can1 / GROUP = cluster 
	MARKERATTRS = (SIZE=9 symbol=CircleFilled);
	STYLEATTRS
	DATACONTRASTCOLORS = (cyan blue orange red bilg magenta yellow);
RUN;


/*export data from PROC CANDISC to file called can_km.xlsx*/
PROC EXPORT DATA=can_km
    OUTFILE="/home/u43389324/Happiness/can_km.xlsx"
    DBMS=xlsx
    REPLACE;
    SHEET="Sheet 1";
RUN;



