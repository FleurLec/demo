
%macro freqcorel(tabin, cible, nbvar, var);
option mprint mlogic;

*11111111111111111111111111111111111111111111111111111111111;
*111    LISTE DES VARIABLES                        111111111;
*11111111111111111111111111111111111111111111111111111111111;	

		data _null_;
		call symput("cible", upcase("&cible."));
		run;

%put &cible. &exclus.;

%if &var. NE  %then
%do;
		data _null_;
		call symput("lstvar_quali", "&var.");
		call symput("nbquali", "&nbvar.");
		run;
%end;
%else 
%do;

	proc contents data=&tabin.   out=contenu(keep=name type) noprint;run;
	data contenu;
	set contenu;
	name=upcase(name);
	if name in ("&cible." &exclus.) then delete;
	run;

	proc transpose data=contenu  out=contenu2(keep=col:);var name;	run;

		data _null_;
		set contenu2;
		Array tab (*) col: ;
			call symput("nbquali", 1*dim(tab));
		run;
		
		data contenu3;
		set contenu2;
		variable=COL1 %do i=2 %to &nbquali.;||" "||COL&i. %end;;
		run;

		data _null_;
		set contenu3(keep=variable);
		call symput("lstvar_quali", variable);
		run;	
%end;

%put &lstvar_quali.  &nbquali.;

*22222222222222222222222222222222222222222222222222222222222;
*222    CRAMER A LA &cible.                          222222222;
*22222222222222222222222222222222222222222222222222222222222;

	   %do i=1 %to &nbquali.;

	    * freq classique;
		proc freq data=&tabin.(where=(&cible.=0) rename=(%scan(&&lstvar_quali,&i," ")=var&i.)) noprint;
		table var&i./out=freq0_&i.(rename=(count=count_0 percent=repart0));
		run;
		proc freq data=&tabin.(where=(&cible.=1) rename=(%scan(&&lstvar_quali,&i," ")=var&i.)) noprint;
		table var&i./out=freq1_&i.(rename=(count=count_1 percent=repart1));
		run;


		data freq_&i.(keep=variable modalite part_cible_1 count_1 count_0) ;
		merge freq0_&i.(in=a) freq1_&i.(in=b);
		by var&i.;
		length variable modalite $32.;
		variable=compress(upcase("%scan(&&lstvar_quali,&i," ")"));
		modalite=trim(left(var&i.));
		if count_1=. then count_1=0;
		if count_0=. then count_0=0;
		part_cible_1=count_1/(count_1+count_0);
		run;


		 * variables qualitatives;
		proc freq data=&tabin.(rename=(%scan(&&lstvar_quali,&i," ")=var&i.)) noprint;
		table var&i.*&cible./chisq;
		output out=outchisq_&i. cramv chisq;
		run;

		data Vcramer_&i.(keep=variable Vcramer Chisq PChisq);
		set outchisq_&i. (keep=_PCHI_ P_PCHI _CRAMV_);
		length variable $32.;
		variable=compress(upcase("%scan(&&lstvar_quali,&i," ")"));
		Vcramer=round(abs(_CRAMV_),0.001);
		Chisq=round(_PCHI_,0.001);
		PChisq=round(P_PCHI,0.001);
		run;

		%end;

		data Vcramer;
		set %do i=1 %to &nbquali.; Vcramer_&i. %end; ;		
		run;

		data freq;
		set %do i=1 %to &nbquali.; freq_&i. %end; ;		
		run;

		proc sort data= Vcramer;by descending Vcramer;run;


*33333333333333333333333333333333333333333333333333333333333;
*333    CRAMER VARIABLES 2 A 2                     333333333;
*33333333333333333333333333333333333333333333333333333333333;	

	data final;
	length var $ 60;
	run;

	*** NOMBRE DE VARIABLES EXPLICATIVES ET MACROS VARIABLES;
	data _null_;
	  set Vcramer end=fin;
	  call symput ('nam'!!left(_n_),variable);
	  if fin then call symput('nbvar',left(_n_));
	run;


	*** PROC FREQ V CRAMER;

	%do i=1 %to %eval(&nbvar-1);

	   %do j=%eval(&i+1) %to &nbvar;

	    proc freq data=&tabin. noprint;
	    tables &&nam&i*&&nam&j /chisq ;
	    output out=t&j cramv;
	    run;

	    data t&j;
		attrib var length=$60;
	    set t&j;
	    variable1="&&nam&i";
	    variable2="&&nam&j";
	    var=compress(variable1||'-'||variable2);
	    run;

	    data final;set final t&j;run;
		proc datasets nolist;delete t&j noprint;run;

	   %end;
	%end;

	data final(keep=var variable1 variable2 vcramer2a2);
	set final;
	vcramer2a2=abs(_cramv_);
	run;

	proc sort data=final out=Vcramer2a2; by descending vcramer2a2; run;

	data Vcramer2a2;
	set Vcramer2a2;
	if Vcramer2a2=. and var="" then delete;
	run;


		proc datasets nolist;
		delete contenu: final %do i=1 %to &nbquali.; outchisq_&i. freq_&i. freq0_&i. freq1_&i. Vcramer_&i. %end;;
		run;

%mend;