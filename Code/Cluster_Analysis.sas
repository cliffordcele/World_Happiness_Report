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


* Correlation matrix/plot;
PROC CORR DATA=dataset_std;
    VAR GDP Support Healthy Freedom Generosity Corruption;
RUN;
PROC SGSCATTER DATA=dataset_std;
   MATRIX GDP Support Healthy Freedom Generosity Corruption / 
   DIAGONAL=(KERNEL HISTOGRAM);
RUN;


ods exclude all;
PROC CORR DATA=dataset_std;   * Call corr proced;
	ODS OUTPUT PearsonCorr=P; * Save corr matrix in dataset P;
	VAR GDP Support Healthy Freedom Generosity Corruption;
RUN;
ods exclude none;

proc print data=P; run; 




/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Cluster Analyses ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


* ~~~~~~~~~~~~~~~~~~~~~ Agglomerative Methods ~~~~~~~~~~~~~~~~~~~~~;

/*--- Agglomerative: macro for clustering and crosstabulating ---*/
* method = ;
* RSQ = ;
* NOSQUARE = ;
* NONORM = ;
* NOEIGEN = ;

%MACRO AgglomCluster(ds, clustMethod, OutName);
	PROC CLUSTER DATA=&ds METHOD=&clustMethod RSQ NOSQUARE NONORM NOEIGEN OUT=&OutName; 
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
PROC TREE DATA = wardout OUT = clus NCLUSTERS = 4; 
	ID CNTRY; COPY GDP Support Healthy Freedom Generosity Corruption;
PROC SORT; 
	BY cluster;
PROC PRINT; 
	BY cluster; 
	VAR GDP Support Healthy Freedom Generosity Corruption; 
RUN;

 


* ~~~~~~~~~~~~~~~~~~~~~ K-MEANS ~~~~~~~~~~~~~~~~~~~~~;

/*--- KMEANS: macro for clustering and crosstabulating ---*/
* radius = Specifies minimum distance for selecting new seeds for inital centroids;
* replace = indicates how inital cluster centroids are selected (none/full/rand/part);
* max clusters = Specifies maximum number of clusters;
* max iterations = Specifies maximum number of iterations for recomputing cluster seed (i.e., initial centroids);
* list = displays cluster assignments for all observations (and dist from final center seed);
* distance = displays distances between cluster centers;

%MACRO FastFreq(ds, radi, replaceProced, numClusters, MaxIt);
	PROC FASTCLUS DATA=&ds RADIUS=&radi REPLACE=&replaceProced MAXCLUSTERS=&numClusters MAXITER=&MaxIt LIST DISTANCE OUT=new;
    	VAR GDP Support Healthy Freedom Generosity Corruption;
    	
	PROC SORT DATA=new; BY cluster;
	PROC PRINT DATA=new; by cluster;
	PROC MEANS MEAN; by cluster;
    	VAR GDP Support Healthy Freedom Generosity Corruption;
	RUN;
%MEND FastFreq;

%FastFreq(dataset_std, 0, random, 4, 0);
%FastFreq(ds=dataset_std, radi=0, replaceProced=none, numClusters=4, MaxIt=100);
%FastFreq(ds=dataset_std, radi=0, replaceProced=full, numClusters=4, MaxIt=100);
%FastFreq(dataset_std, 0, full, 4);
%FastFreq(dataset_std, 5, none, 4);
%FastFreq(dataset_std, 5, full, 4);
























/*--- KNN (FIX LATER): macro for clustering and crosstabulating ---*/
/*--- cluster membership with species ---*/
%MACRO FastFreq(ds, numClusters);
   PROC FASTCLUS DATA=&ds OUT=clust MAXCLUSTERS=&numClusters MAXITER=100 NOPRINT;
      var GDP_per_capita Social_Support Healthy_Life Freedom_Choices Generosity Corruption;
   RUN;

   PROC FREQ DATA=clust; TABLES Country*cluster; RUN;
%MEND FastFreq;






%Std(STD);
%FastFreq(sdzout);


/*Hierarchical methods */
proc cluster data = multi.sport method = single rsq nosquare nonorm noeigen out = singleout;
id SPORT; 
proc sort; by _ncl_;


proc cluster data = multi.sport method = complete rsq nosquare nonorm noeigen out = completeout;
id SPORT;
proc sort; by _ncl_;


proc cluster data = multi.sport method = centroid rsq nosquare nonorm noeigen out = centroidout;
id SPORT;
proc sort; by _ncl_;


proc cluster data = multi.sport method = ward rsq nosquare nonorm noeigen out = wardout;
id SPORT;
proc sort; by _ncl_;





/* OLD Agglo METH*/
*Method - Single linkage (nearest neighbor);
PROC CLUSTER DATA=dataset_std METHOD=single RSQ NOSQUARE NONORM NOEIGEN out=singleout; 
	VAR GDP_per_capita Social_Support Healthy_Life Freedom_Choices Generosity Corruption; 
	ID CNTRY; 
PROC SORT; BY _ncl_;

*Method - Complete Linkage (Furthest Neighbor);
PROC CLUSTER DATA=dataset_std METHOD=complete RSQ NOSQUARE NONORM NOEIGEN out=completeout; 
	VAR GDP_per_capita Social_Support Healthy_Life Freedom_Choices Generosity Corruption; 
	ID CNTRY;  
PROC SORT; BY _ncl_;
	
*Method - Centroid;
PROC CLUSTER DATA=dataset_std METHOD=centroid RSQ NOSQUARE NONORM NOEIGEN out=centroidout; 
	VAR GDP_per_capita Social_Support Healthy_Life Freedom_Choices Generosity Corruption; 
	ID CNTRY;  
PROC SORT; BY _ncl_;

*Method - Ward’s error;
PROC CLUSTER DATA=dataset_std METHOD=ward RSQ NOSQUARE NONORM NOEIGEN out=wardout; 
	VAR GDP_per_capita Social_Support Healthy_Life Freedom_Choices Generosity Corruption; 
	ID CNTRY; 
PROC SORT; BY _ncl_;

*Method - Average;
PROC CLUSTER DATA=dataset_std METHOD=average RSQ NOSQUARE NONORM NOEIGEN out=averageout; 
	VAR GDP_per_capita Social_Support Healthy_Life Freedom_Choices Generosity Corruption; 
	ID CNTRY; 
PROC SORT; BY _ncl_;

* Create dendrograms;
PROC TREE DATA=singleout;   ID CNTRY;
PROC TREE DATA=completeout; ID CNTRY;
PROC TREE DATA=centroidout; ID CNTRY;
PROC TREE DATA=wardout;     ID CNTRY;
PROC TREE DATA=averageout;  ID CNTRY;

*Adding R sq values for each method to new variable;
DATA singleout;   SET singleout;   single = _rsq_;
DATA completeout; SET completeout; complete = _rsq_;
DATA centroidout; SET centroidout; centroid = _rsq_;
DATA wardout;     SET wardout;     ward = _rsq_;
DATA averageout;  SET averageout;  average = _rsq_;

*Merging all output values for all methods into one dataset sorted by # Clusters;
DATA outputs; MERGE singleout completeout centroidout wardout averageout; BY _ncl_;
DATA outputs; SET outputs; IF _ncl_ <20;

symbol1 i = join v = S l = 1  c = gray; 
symbol2 i = join v = P l = 5  c = blue;
symbol3 i = join v = C l = 10 c = orange; 
symbol4 i = join v = W l = 15 c = red;
symbol5 i = join v = A l = 20 c = CX00FF00;

PROC GPLOT DATA = outputs;
PLOT single*_ncl_=1 complete*_ncl_=2 centroid*_ncl_=3 ward*_ncl_=4 average*_ncl_=5 /overlay legend;
RUN;

















