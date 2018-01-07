(* ::Package:: *)

(*Protect information*)Attributes[spiral]={ReadProtected};
spiral::usage="spiral[name,t1,t2,options]\nname: the directory of the file;\nt1: starting time;\nt2: time duration;\nmode->1 or 2;\nimagesize: (Automatic; customized);\nclockwise: Making a clockwise;\nMODE1:\nobvious: the obious of peaks, usually from 20 to 60;\ncontrollable: if True, then return a controllable scope, otherwise a clear graphic;\nphase: analyze phase (True, False; point placement: Top, Bottom);\nstereo: (True, False; Left, Right);\nMODE2:\nrange: plot within a certain range;\ncrossing: showing pitch number;";spiral[ipt___,OptionsPattern[]]:=Module[{md1,md2},Clear[obvious,controllable,clockwise,phase,imagesize,stereo,range,crossing,mode];Options[spiral]={obvious->30,controllable->False,clockwise->True,phase->False,imagesize->Automatic,stereo->True,range->5,crossing->True,mode->1};
(*spiral scope*)md1:=Module[{logdata,lineardata,data,sprt,t,t0,t1,par,file,lengh,r,\[Theta],mx,ect,finaldata,finaldata1,pno,abc,s,s3d,plots,txt,idct,arg,arg2,abc1,args,argtm,vlm,fqc,ctscp,uzdata,input,name,x1,x2,lg,four, invfour,cw,halflg,dat,stroarg,stro},
input={ipt};lg=Length@input;name=If[lg!=0,First@input,"rbk"];
(*switch modes*)Which[lg==1,x1=x2=0;,lg==2,x1=input[[2]];x2=0;,lg==3,x1=input[[2]];x2=input[[3]];,True,x1=0;x2=0;];
file=If[FileExistsQ@name,First@Import[name,"Sound"]];(*Import data*)If[FileExistsQ@name,dat=First@file;data=If[Length@dat>1,Which[ToString@OptionValue[stereo]=="True",dat,ToString@OptionValue[stereo]=="Left",{dat[[1]]},ToString@OptionValue[stereo]=="Right",{dat[[2]]},ToString@OptionValue[stereo]=="False",{dat[[#]]}&@RandomChoice[{1,2}]],dat];
(*sample Rate*)sprt=file[[2]];,data={Table[RandomInteger[],{5}]};sprt=44100;];
(*time function*)t[t_]:=Round[t*sprt];
lengh=Length@First@data;
(*start time*)t0=t@x1;t0=If[x1==0,1,If[t0>lengh||x1<0,lengh-4,t0]];
(*end time*)t1=t@(Abs@x2+x1);t1=If[x2==0||t1>lengh||x1<0,lengh,t1];
(*Use part of data*)uzdata=data[[;;,t0;;t1]];
(*Color Obvious*)par=1.2;
(*spiral eccentricity*)ect=1.02;
(*Piano keys*)pno=RotateRight[{0,1,0,1,0,0,1,0,1,0,1,0},2];
(*the name of notes*)abc={0->"a",1->"a#",2->"b",3->"c",4->"c#",5->"d",6->"d#",7->"e",8->"f",9->"f#",10->"g",11->"g#",12->"a"};
(*Sound Function*)s[x_,y_]:=(s3d=If[x==0,0,1.3 RandomReal[{-1,1}]];EmitSound[Play[{y*2^(-1*^3 t^2) Sin[x 2.\[Pi] t],y*2^(-1*^3 t^2) Sin[x 2.\[Pi] t+s3d]},{t,-.01,.1},PlayRange->{-1,1},SampleRate->sprt]]);
(*half of length for fourier transformation*)halflg=IntegerPart[(t1-t0)/2];
(*fourier transformation*)four=((Norm/@#)&/@Fourier/@uzdata)[[;;,;;halflg]];
(*Phase Analysis*)stro:=(stroarg=(-cw*(Arg/@Fourier[uzdata[[1]]]-Arg/@Fourier[uzdata[[2]]])[[;;halflg]])/(2\[Pi]););
(*Inverse fourier for sound play*)invfour:=If[Length@uzdata[[1]]<10,uzdata,(Re[InverseFourier/@four])[[;;,;;Round[Length[four[[1]]]/2]]]];
(*clockwise spiral*)cw=If[OptionValue[clockwise],-1,1];
logdata=(OptionValue[obvious]/100+1)^Log[2,#+10^-6]&@Total[four];logdata=logdata-Min@logdata;logdata=logdata/Max@logdata;lineardata=Table[{i/((t1-t0)/100. )*sprt/44100,logdata[[i]]},{i,Length@logdata}];

finaldata=Table[(({cw*r Cos[\[Theta]],r Sin[\[Theta]]}/.r->(ect^\[Theta]+0.95(ect^(\[Theta]+2\[Pi])-ect^\[Theta])*lineardata[[i,2]]))/.\[Theta]->2\[Pi] Log[2,lineardata[[i,1]]]),{i,Length@lineardata}];
finaldata1=Table[(({cw*r Cos[\[Theta]],r Sin[\[Theta]]}/.r->ect^\[Theta])/.\[Theta]->2\[Pi] Log[2,lineardata[[i,1]]]),{i,Length@lineardata}];
mx=Norm[({cw*r Cos[\[Theta]],r Sin[\[Theta]]}/.r->ect^\[Theta])/.\[Theta]->2\[Pi]+2\[Pi] Log[2,lineardata[[Length@lineardata,1]]]];

(*main scope*)plots:=Show[{Graphics[Flatten@{Thick,Circle[{0,0},mx],EdgeForm[Thick],GrayLevel[.9],Dashed,(*outside circle*)Table[{Line[mx{{0,0},{cw*Cos[i 2\[Pi]/12],Sin[i 2\[Pi]/12]}}]},{i,12}],(*piano keys*)Dashing[None],Thickness[.006],Table[{If[pno[[i]]==0,LightGray,Black],Line[{mx{cw*Cos[i 2\[Pi]/12],Sin[i 2\[Pi]/12]},.987mx{cw*Cos[i 2\[Pi]/12],Sin[i 2\[Pi]/12]}}]},{i,12}],(*hue disk*)Table[{Lighter@Hue[(i-3)/12],Disk[mx{cw*Cos[i 2\[Pi]/12],Sin[i 2\[Pi]/12]},.014mx]},{i,12}]}],(*Spiral Guideline*)ParametricPlot[{cw*r Cos[\[Theta]],r Sin[\[Theta]]}/.r->ect^\[Theta],{\[Theta],-100,2\[Pi] Log[ 2,lineardata[[-1,1]]]},PlotStyle->Directive[GrayLevel[.95],Dashed],PlotRange->All,Axes->None],(*frequency line and spiral*)Graphics@Flatten@{Thick,Table[{GrayLevel[1-par*lineardata[[i,2]]],Line[{finaldata[[i]],finaldata1[[i]]}],GrayLevel[1-par*(lineardata[[i,2]]+lineardata[[i+1,2]])/2],Line[{finaldata[[i]],finaldata[[i+1]]}]},{i,2,Length@finaldata-1}],(*Hue for Phase*)If[Length@data>1&&ToString@OptionValue[phase]!="False",stro;SortBy[Table[{lineardata[[i,2]],Lighter[Darker[Hue[par*stroarg[[i]]],.8(1-lineardata[[i,2]])],1-par*lineardata[[i,2]]],PointSize[.0009*6^lineardata[[i,2]]],Point[Which[(ToString[OptionValue[phase]]=="Bottom")||(ToString@OptionValue[phase]=="True"),finaldata1[[i]],ToString@OptionValue[phase]=="Top",finaldata[[i]]]]},{i,2,Length@finaldata-1}],First][[;;,2;;]],{}]},(*440hz tick*)Graphics[{Thick,Circle[{cw*1,0},.015]}]},ImageSize->If[ToString@OptionValue[imagesize]==ToString@Automatic,1600,OptionValue[imagesize]]];
(*Indicator*)idct:=Graphics[Dynamic[{(*Text*)txt=MousePosition["Graphics",{cw,0}];
arg=Mod[Arg[cw*txt[[1]]+txt[[2]]*I],2\[Pi]];
arg2=Mod[Arg[cw*txt[[1]]+txt[[2]]*I]-\[Pi]/2+\[Pi]/12,2\[Pi]];args=Mod[6/\[Pi] arg,12];
abc1=5+Floor@((Log[ect,Norm[txt]/ect^(\[Pi]/2-\[Pi]/12)]-arg2)/(2 \[Pi]));
argtm=((Log[ect,Norm[txt]]-arg)/(2 \[Pi]));
(*volume*)vlm=FractionalPart[Mod[argtm,1]];
(*frequency*)fqc=2^Floor[argtm]*440*2^(args/12);Text[Column[{(Round@args/.abc)<>ToString[abc1]<>"::"<>ToString[Round[100(-Round@args+args)]]<>"\[Cent]",ToString[Ceiling[fqc,0.001]]<>"Hz",ToString[Ceiling[100*vlm]]<>"%"},Center,Frame->True,Background->White],{0,0}]}]];(*Controllable scope*)ctscp:=EventHandler[Deploy@Show[{plots,idct},ImageSize->If[ToString@OptionValue[imagesize]==ToString@Automatic,800,OptionValue[imagesize]]],"MouseDown":>Which[fqc<=0.1,EmitSound[ListPlay[uzdata,SampleRate->sprt,PlayRange->All]],0.1<fqc<1,EmitSound[ListPlay[invfour,SampleRate->Round[sprt/2]]],True,s[fqc,vlm]]];If[OptionValue[controllable],invfour;ctscp,plots]];
(*****************************************************************************************************)
(*stereo scope*)md2:=Module[{input,lg,name,x1,x2,file,dat,data,sprt,t,lengh,t0,t1,uzdata,pno,abc,halflg,four,pc,s,fours,lengh2,stroarg,fourmagdis,freq,logfreq,lgmx,lgmn,pnts,mx,rg,latis,grh,rdm,pctk,deg,lgrd,smrd,dif,cr,dif2,rto,lin,lins,cw},
input={ipt};lg=Length@input;name=If[lg!=0,First@input,"rbk"];
(*switch modes*)Which[lg==1,x1=x2=0;,lg==2,x1=input[[2]];x2=0;,lg==3,x1=input[[2]];x2=input[[3]];,True,x1=0;x2=0;];
file=If[FileExistsQ@name,First@Import[name,"Sound"]];(*Import data*)If[FileExistsQ@name,dat=First@file;data=dat[[;;2]];
(*sample Rate*)sprt=file[[2]];,data=RandomInteger[1,{5,2}];sprt=44100;];
(*time function*)t[t_]:=Round[t*sprt];
lengh=Length@First@data;
(*start time*)t0=t@x1;t0=If[x1==0,1,If[t0>lengh||x1<0,lengh-4,t0]];
(*end time*)t1=t@(Abs@x2+x1);t1=If[x2==0||t1>lengh||x1<0,lengh,t1];
(*Use part of data*)uzdata=data[[;;,t0;;t1]];
(*clockwise spiral*)cw=If[OptionValue[clockwise],1,-1];
(*Piano keys*)pno=RotateRight[{0,1,0,1,0,0,1,0,1,0,1,0},2];
(*the name of notes*)abc={0->"a",1->"a#",2->"b",3->"c",4->"c#",5->"d",6->"d#",7->"e",8->"f",9->"f#",10->"g",11->"g#",12->"a"};
(*pitch name generator*)pc[x_]:=((Mod[Round[Log[2.,x/440] 12],12])/.abc)<>ToString[IntegerPart[Log[2.,x/(440*2^(2.5/12))] +5]]<>"::"<>ToString[Ceiling[100(-Round[Log[2.,x/440] 12]+Log[2.,x/440]12 ),0.01]]<>"\[Cent]";(*Sound Function*)s[x_,y_]:=(EmitSound[Play[{2^(-1*^3 t^2) Sin[x 2.\[Pi] t],2^(-1*^3 t^2) Sin[x 2.\[Pi] t+y]},{t,-.01,.1},PlayRange->{-1,1},SampleRate->sprt]]);
(*half of length for fourier transformation*)halflg=IntegerPart[(t1-t0)/2];
(*fourier transformation*)four=((Norm/@#)&/@Fourier/@uzdata)[[;;,;;halflg]];
fours=Total[four^2];
lengh2=Length@fours;
(*Phase Analysis: to angle*)stroarg=(10^-6RandomReal[]-(Arg/@Fourier[uzdata[[1]]]-Arg/@Fourier[uzdata[[2]]])[[;;halflg]]);
(*magnitude difference of Left and right ear: to GrayLevel edge*)fourmagdis=four[[2]]-four[[1]]+10^-6RandomReal[];
fourmagdis=fourmagdis/Max[Abs[fourmagdis]];
(*generate frequency information*)freq=Table[i/((t1-t0)/100. )*sprt/44100,{i,lengh2}];
logfreq=Log[2,freq];
lgmx=Max@logfreq-1;lgmn=(1-Min@logfreq);
(*Plot*)
pnts=Table[{1/fours[[i]],1/fours[[i]]{Cos[#+\[Pi]/2],Sin[#+\[Pi]/2]}}&@stroarg[[i]],{i,lengh2}];
mx=Max[Norm/@pnts[[;;,2]]];
(*range*)rg=OptionValue[range];
(*Large and Small radius*)
lgrd=rg*0.008;smrd=0.8*lgrd;rto=0.8;
(*how many tick*)latis=5;
(*show crossing indicators or not*)cr=OptionValue[crossing];
(*outside circle*)
rdm:=Mod[-Round[12*cw*(logfreq[[i]]-1/4)]/12*2\[Pi]+\[Pi]/2,2\[Pi]];
dif:=-cw*(logfreq[[i]]-Round[12*logfreq[[i]]]/12)*2\[Pi]*24;
dif2:=2\[Pi] fourmagdis[[i]];
(*Graphics*)
grh:=Graphics[Flatten@{Thin,GrayLevel[.9],Dashed,
(*edge*)Table[Circle[{0,0},rg i/latis],{i,latis}],
(*ticks*)Table[Line[rg{{Cos[\[Theta]],Sin[\[Theta]]},{Cos[\[Theta]+\[Pi]],Sin[\[Theta]+\[Pi]]}}],{\[Theta],0,2\[Pi],1/6 \[Pi]}],Thin,Dashing[None],
(*under lines*)lins:=(lin=DeleteCases[Table[If[pnts[[i,1]]<rg,{Mod[Round[Log[2.,freq[[i]]] 12],12],pnts[[i,2]]},{}],{i,lengh2}],{}];Flatten@{Thin,DeleteCases[Table[If[lin[[i,1]]==lin[[i+1,1]],{Lighter[Hue[lin[[i,1]]/12-1/4],.8],Line[{lin[[i,2]],lin[[i+1,2]]}]},{}],{i,Length@lin-1}],{}]});If[cr,lins,{}],
(*pitch ticks*)If[cr,Table[If[pnts[[i,1]]<rg,pctk=IntegerPart[Log[2.,(440*freq[[i]])/(440*2^(2.5/12))] +5];
If[pctk>0,{(*tick for difference circle; each tick represent 10 cents*)LightGray,Thin,Table[Line[{pnts[[i,2]],pnts[[i,2]]+smrd{Cos[j+rdm],Sin[j+rdm]}}],{j,0,2\[Pi]-2\[Pi]/5,2\[Pi]/5}],(*pitch tick*)Table[{If[j==0,{Black,Thickness[Medium]},{Hue[logfreq[[i]]-1/4],Thickness[Small]}],Line[{pnts[[i,2]],pnts[[i,2]]+lgrd{Cos[j+rdm],Sin[j+rdm]}}]},{j,0,2\[Pi]-2\[Pi]/pctk,2\[Pi]/pctk}]}],{}],{i,lengh2}],{}],
(*piano keys*)Dashing[None],Thickness[.006],
Table[{If[pno[[i]]==0,LightGray,Black],Line[{rg{-cw*Cos[i 2\[Pi]/12],Sin[i 2\[Pi]/12]},.987rg{-cw*Cos[i 2\[Pi]/12],Sin[i 2\[Pi]/12]}}]},{i,12}],
(*pitch cents difference circle & Left Right difference circle*)If[cr,{Black,Thin,Table[If[pnts[[i,1]]<rg,{Circle[pnts[[i,2]],smrd,{rdm,rdm+dif}],Circle[pnts[[i,2]],rto*smrd,{rdm,rdm+dif2}]},{}],{i,lengh2}]},{}],
(*points*)Table[If[pnts[[i,1]]<rg,{If[logfreq[[i]]>1,Lighter[#,(logfreq[[i]]-1)/lgmx],Darker[#,(1-logfreq[[i]])/lgmn]]&@Hue[logfreq[[i]]-1/4],Tooltip[#,Column[{pc[440*freq[[i]]],ToString[440*freq[[i]]]<>"Hz",If[fourmagdis[[i]]>0,"R","L"]<>ToString[Round[Abs[fourmagdis[[i]] 100]]]<>"%::"<>(deg=Mod [-stroarg[[i]]/\[Pi] 180,360];If[deg>180,"L","R"]<>ToString[Round@If[deg>180,-deg+360,deg]])<>"\[Degree]"},Center]]&@With[{i=i},Button[Disk[pnts[[i,2]],.975 rto*smrd],s[440*freq[[i]],Mod[stroarg[[i]],2\[Pi]]]]]},{}],{i,lengh2}],
(*outside frame*)Black,Thick,Circle[{0,0},rg],
(*Hue disk*)EdgeForm[Directive[Black,Thick]],Table[{Lighter@Hue[-\[Pi]/4-\[Theta]/\[Pi]/2],Disk[rg{cw*Cos[\[Theta]],Sin[\[Theta]]},.014rg]},{\[Theta],0,2\[Pi],1/6 \[Pi]}]},ImageSize->If[ToString@OptionValue[imagesize]==ToString@Automatic,1600,OptionValue[imagesize]]];grh];
(*switch among different scopes*)
Which[OptionValue[mode]==1,md1,OptionValue[mode]==2,md2,True,Beep[]]]


(*Protect information*)Attributes[star]={ReadProtected};
star::usage="spiral[name,t1,t2,options]\nname: the directory of the file;\nt1: starting time;\nt2: time duration;\nimagesize: the size of image;\nrange: the range of graphics for plot;\nsize: the size of stars;\nnight: nightmode (True, False);\ngrid: gridlines (True, False);\nselection: select the range of frequencies (0~90);\nindicator: the indicator of the dots;\nopacity: the opacity of the dots;\nstarlines: the star lines between stars;";star[name_,t0_,time_,OptionsPattern[]]:=Module[{dat,lg,sprt,tstrt,tend,sp,lgth,w,fftsp,argsp,magsp,rtmagsp,tmag,tpt,v,mn,a,f0,pos,b,x,y,pr,length,abc,rt1,b1,r1,r2,s,phs1,f1,lgth1,w0},Clear[imagesize,range,size,night,grid,selection,indicator,opacity,starlines];Options[star]={imagesize->1200,range->10,size->4,night->True,grid->False,selection->1,indicator->True,opacity->1,starlines->False};
abc={0->"c",1->"c#",2->"d",3->"d#",4->"e",5->"f",6->"f#",7->"g",8->"g#",9->"a",10->"a#",11->"b"};
(*play sound*)s[f_,vol_,phs_]:=EmitSound@Play[{vol[[2]]Sin[2\[Pi] f t],vol[[1]]Sin[2\[Pi] f t-phs]},{t,0,.2},SampleRate->sprt,PlayRange->{-1,1}];
dat=Import[name,"Sound"];
lg=Length[dat[[1,1,1]]];
sprt=dat[[1,2]];
tstrt=If[#==0,1,#]&@Floor[sprt*t0];
tend=If[#>lg,lg,#]&@Floor[sprt*t0+sprt*time];
sp=dat[[1,1,;;,tstrt;;tend]];
length=Length[First[sp]];
lgth=Floor[length/2];
w=(Range[lgth]-1)*sprt/length;(*minimum frequency error*)w0=w[[2]]-w[[1]];
fftsp=(Fourier[#]&/@sp)[[;;,;;lgth]];
argsp=Mod[First@Differences[Arg/@fftsp]+\[Pi],2\[Pi]]-\[Pi];
magsp=Abs/@fftsp;magsp=magsp/Max[magsp];
rtmagsp=Quiet[magsp[[1]]/magsp[[2]]]/.{Indeterminate->10^8,ComplexInfinity->10^8};
tmag=Total@magsp;tmag=tmag/Max@tmag;
(*temperature*)tpt=20;
(*sound speed*)v=(331.3+0.606*tpt);
(*distance between two ears*)a=0.215;a=a/2;
(*marginal frequency of phase*)f0=v/(4a);
mn=0.01*OptionValue[selection];(*image range*)pr=1.*OptionValue[range];
(*Solve Equations*)
r1=Re@(Solve[{Sqrt[(x+a)^2+y^2]-Sqrt[(x-a)^2+y^2]==b1,(*Tan[ArcCos[b1/(2 a)]]x-y\[Equal]0,*)((x+a)^2+y^2)/((x-a)^2+y^2)==rt1^(1.+10^-10)},{x,y}][[2,;;,2]]);
r2=Re@(Solve[{Tan[Sign[phs1]((-4 a f1 \[Pi]+v Abs[phs1])/(8 a f1-2 v))]x-y==0,((x+a)^2+y^2)/((x-a)^2+y^2)==rt1^1.},{x,y}][[1,;;,2]]);
pos[f_,phs_,rt_,tmg_,mg_]:=(
b=v/f*phs/(2\[Pi]);
(*ContourPlot[{Sqrt[(x+a)^2+y^2]-Sqrt[(x-a)^2+y^2]\[Equal]b,(((x+a)
)^2+y^2)/((x-a)^2+y^2)\[Equal]rt,Tan[ArcCos[b/(2 a)]]x-y\[Equal]0},{x,-10,10},{y,-10,10},PlotPoints\[Rule]50]*)
If[tmg>mn,{(*1*)f*1.,(*2*)If[Max@Abs@#>pr,pr*#/Max@Abs@#,#]&@If[Element[ArcCos[b/2/a],Reals],{#[[1]],Abs[#[[2]]]}&@r1/.{rt1->rt,b1->b},{#[[1]],-Abs[#[[2]]]}&@r2/.{rt1->rt,b1->b,phs1->phs,f1->f}],(*3*)tmg^.5,(*4*)Row@{IntegerPart[Mod[#+.5,12]]/.abc,Floor[(#+.5)/12]+4,"::",100*(FractionalPart[Mod[#+.5,12]]-.5),"\[Cent]"}&@(12*Log[2.,f/(440*2^(-9/12))]),(*5*)Element[ArcCos[b/2/a],Reals],(*6*){mg,phs}},{}]);
DynamicModule[{aaa,ii},((*process indicator*)ii=0;PrintTemporary[Dynamic@If[ii!=0.,ProgressIndicator[ii,ImageSize->{200,5}],ProgressIndicator[Appearance->"Indeterminate",ImageSize->{200,5}]]];
(*extract information*)aaa=Quiet@Table[ii=i/lgth;pos[w[[i]],argsp[[i]],rtmagsp[[i]],tmag[[i]],magsp[[;;,i]]],{i,lgth}];
aaa=DeleteCases[aaa,{}][[2;;]];lgth1=Length@aaa;Deploy@Graphics[Flatten@{GrayLevel[If[OptionValue[night],.13,.95]],(*ear*)Disk[{0,0},a],Disk[{a,0},a/2],Disk[{-a,0},a/2],Thin,GrayLevel@If[OptionValue[night],.2,.8],Dashed,(*grid lines*)Line[{{-pr,0},{pr,0}}],Line[{{0,-pr},{0,pr}}],If[OptionValue[night],White,Black],Point[{a,0}],Point[{-a,0}],If[OptionValue[opacity]==1,{},Opacity[OptionValue[opacity]]],Dashing[None],If[OptionValue[starlines],ii=0;Table[{(*color of star lines*)If[OptionValue[night],Darker[#,.8],Lighter[#,.8]]&@Hue[Log[2,aaa[[i,1]]/(440*2^(-9/12))]],(*star lines*)If[i>1,If[aaa[[i,1]]-aaa[[i-1,1]]<=2*w0&&Max[aaa[[i-1,2]]]!=pr,Line[{aaa[[i,2]],aaa[[i-1,2]]}]],{}]},{i,Length@aaa}],{}],Table[ii=1-i/lgth1;{(*color of disk*)If[tmp=Log[2,aaa[[i,1]]/440];tmp>0,Lighter[#,tmp/5],Darker[#,1-((5+tmp)/5)]]&@Hue[Log[2,aaa[[i,1]]/(440*2^(-9/12))]],(*disk indicator*)If[OptionValue[indicator],Tooltip[#,Column@{Row[{aaa[[i,4]],", ",aaa[[i,1]],"Hz"}],Row[Flatten@{If[MemberQ[Abs[aaa[[i,2]]],pr],"(\[Infinity]) ",Norm[aaa[[i,2]]]],"m::",If[#<0,{"L,",-#},{"R,",#}]&@(180/\[Pi]*If[aaa[[i,2,2]]<0,If[aaa[[i,2,1]]<0,#-\[Pi],\[Pi]+#],#]&@ArcTan[aaa[[i,2,1]]/(If[#<10^-6,10^-6,#]&@aaa[[i,2,2]])]),"\[Degree]"}],Row[{100*aaa[[i,6,1,2]],"::",100*aaa[[i,6,1,1]],"%Pa"}]},TooltipDelay->.2]&@With[{i=i},Button[#,s[aaa[[i,1]],aaa[[i,6,1]],aaa[[i,6,2]]]]],#]&@(*disk*)Disk[aaa[[i,2]],.004pr OptionValue[size]aaa[[i,3]]],If[OptionValue[night],Black,White],If[!aaa[[i,5]],Disk[aaa[[i,2]],.004 pr*.6OptionValue[size]aaa[[i,3]]]]},{i,Length@aaa}]},Frame->True,FrameStyle->If[OptionValue[night],LightGray,Automatic],PlotRange->pr,ImageSize->OptionValue[imagesize],Background->If[OptionValue[night],Black,White],GridLines->If[OptionValue[grid],Automatic,None],GridLinesStyle->Directive[GrayLevel[If[OptionValue[night],.2,.9]]],ImageMargins->5])]]


(*Protect information*)Attributes[grass]={ReadProtected};
Clear["Global`*"];
grass::usage="grass[x,Patterns]
x can either be list of freqencies in ratio form (e.g. {1,5/4,3/2});
also can be two lists in comparison (e.g. {{1,5/4,3/2},3/2*{1,5/4,3/2}});
Pattern:: True or False:
nightmode: Dark theme;
overtone: The number of overtones;
frequency: Sound play frequencies of 1;
text: Text label on fundamental frequency;
pitch: Other input type  (e.g. {\"c4\",\"e4\",\"g4\"},{\"g4\",\"b4\",\"d5\"}} or {{0,4,7},{0,4,7}+7});
temperment: Different temperment ticks;
tick: Tick of indicator";
grass[x_?ListQ,OptionsPattern[]]:=Module[
{avN,cal,col,coord,cpraw,crd,crd12,crddat,crdu,dat,grh,gtcrd,hf,huex,lcrd,lcrdu,leng,ln,ln2,lnbs,lncrd,lnN,logdat,lokup,ltr,mn,mx,mxag,mxplt,shake,org,ovr,ovrn,pnt,rawcrd,rlkup,rtio,rtioC,s,same,tf,txtQ,ticks,tipcord},
Options[grass]={nightmode->False,overtone->2^5-1,frequency->440 2.^(-9/12),text->True,pitch->False,temperment->12,tip->True};
(*sound*)
s[y_]:=If[OptionValue[frequency]*y>20000,Null,EmitSound@Play[Sin[If[txtQ,440. 2^(-9/12),OptionValue[frequency]] 2 \[Pi] t y],{t,0,.2},SampleRate->44100]];
(*build library*)ltr={"c","c#","d","d#","e","f","f#","g","g#","a","a#","b"};lokup=Flatten@Table[ltr[[i]]<>ToString[j]->12*j+i-49,{i,1,12},{j,0,9}];
rlkup=Flatten@Table[12*j+i-49->ltr[[i]]<>ToString[j],{i,1,12},{j,0,9}];
(*maximum overtone*)ovrn=OptionValue[overtone];
(*average temperment*)avN=OptionValue[temperment];
(*coefficient of shorten overtones*)leng=1.1;
(*left-right shake*)shake=.9;
(*maximum tilt*)mxag=\[Pi]/3;
(*background color*)col=If[OptionValue[nightmode],Black,White];
crddat=If[Depth[x]==2,{x},If[Depth[x]==3&&Length@x<=2,x,{{1}}]];
(*Chord data*)rawcrd=Union/@crddat;(*copy data to show in tooltip*)cpraw=Flatten@rawcrd;
(**convert chord data**)
txtQ=And@@(StringQ[#]&/@Flatten[rawcrd]);
cal:=(rawcrd=2.^(#/12)&@If[txtQ,Map[ToString,rawcrd,{Depth[rawcrd]-1}]/.lokup,rawcrd]);
If[txtQ,cpraw=Sort[Flatten[rawcrd/.lokup]]/.rlkup;cal;,If[OptionValue[pitch],cal,Null]];
(*tell type*)tf=Length@rawcrd>1;
crd=Sort[Flatten@rawcrd];
crdu=Union@crd;
lcrd=Length@crd;
lcrdu=Length@crdu;
(*create gather index*)
gtcrd=Gather[crd];
crd12=If[tf,-1,1]*Flatten@Table[If[Length[gtcrd[[i]]]==1,If[MemberQ[rawcrd[[1]],gtcrd[[i,1]]],1,-1],{1,-1}],{i,Length@gtcrd}];
(*same chord?*)same=Flatten@Table[If[Length@gtcrd[[i]]==2,{True,True},False],{i,Length@gtcrd}];
huex=Flatten@Table[Position[crdu,crd[[i]]],{i,Length@crd}]-1;
(*generate overtone chord*)
ovr=Range[ovrn];
dat=ovr*#&/@crd;
(*maximum range*)mxplt=dat[[1,-1]];
(*convert to pitch*)logdat=Log[2.,dat];
mx=Floor@logdat[[1,-1]]+If[Length@crddat==1,1,0];
mn=Floor@logdat[[1,1]];
rtio=12*GoldenRatio;
hf=If[tf,1/2,1];
rtioC=hf*rtio;
(*coordinates*)coord=Table[Table[{Mod[#,1],N@Floor[#]/rtioC}&@logdat[[j,i]],{i,ovrn}],{j,Length@crd}];
(*Base line*)ln=Table[Line[{{0,i/rtioC},{1,i/rtioC}}],{i,mn,mx}];
(*separator of up-down*)ln2=Table[Line[{{0,(i+.5)/rtioC},{1,(i+.5)/rtioC}}],{i,mn-1,mx}];
(*average temperment tips*)lnN=Table[{Line[{{i/avN,If[tf,(mn-.5)/rtioC,mn/rtioC]},{i/avN,If[tf,(mx+.5)/rtioC,mx/rtioC]}}]},{i,0,avN}];
(*Chord line*)lncrd=Table[{(*color*)Hue[huex[[j]]/lcrdu],Table[If[dat[[j,i]]>mxplt,{},#]&@{tipcord=coord[[j,i]]+hf*(leng^-i)/rtioC*crd12[[j]]*{If[OddQ@i,1,-1]*Cos[(1-shake^(i-1))*mxag+\[Pi]/2],Sin[(1-shake^(i-1))/ovrn*mxag+\[Pi]/2]};(*main*)If[same[[j]],Dashed,Dashing[None]],Tooltip[#,Row@Flatten@{cpraw[[j]],"::",Framed[i,RoundingRadius->5,FrameStyle->None,Background->Lighter[Hue[huex[[j]]/lcrdu],.9]],If[txtQ,{"::",N[440 2.^(-9/12)*dat[[j,i]]],"Hz"},If[OptionValue[pitch],"",{"::",cpraw[[j]]*i}]]},TooltipDelay->0.6,TooltipStyle->{Background->Lighter[Hue[huex[[j]]/lcrdu],.8]}]&@With[{i=i,j=j},Button[#,s[dat[[j,i]]]]]&@Line[{coord[[j,i]],tipcord}],(*tip disk*)Disk[tipcord,.03/rtio],Dashing[None],
If[OptionValue[tip],#,Null]&@{(*tip indicator*)ticks=Floor[Mod[i,10]/2];(*tip dots*)If[11<=i<31,Table[Disk[tipcord+.06/rtio*crd12[[j]]*{If[OddQ@i,1,-1]*Cos[(1-shake^(i-1))*mxag-\[Pi]/2+2\[Pi] k/(ticks+1)],Sin[(1-shake^(i-1))*mxag-\[Pi]/2+2\[Pi] k/(ticks+1)]},.015/rtio],{k,1,ticks}]],(*tip circle*)If[i<=11,Circle[tipcord,.06/rtio]],(*tipline*)If[i<=20,Table[Line[{#,#+.06/rtio*crd12[[j]]*{If[OddQ@i,1,-1]*Cos[(1-shake^(i-1))*mxag-\[Pi]/2+2\[Pi] k/(ticks+1)],Sin[(1-shake^(i-1))*mxag-\[Pi]/2+2\[Pi] k/(ticks+1)]}}&@tipcord],{k,1,ticks}],Null]}},{i,2,ovrn}]},{j,lcrd}];
(*Base Chord Line*)lnbs=Table[If[dat[[j,1]]>mxplt,{},#]&@{If[OptionValue[nightmode],Lighter,Darker]@Hue[huex[[j]]/lcrdu],Disk[coord[[j,1]]+crd12[[j]]*{0,1/rtio},.04/rtio],Dashing[None],Circle[coord[[j,1]],.05/rtio],Circle[coord[[j,1]]+crd12[[j]]*{0,1/rtio},.08/rtio],If[same[[j]],Dashed,Dashing[None]],Tooltip[#,Row@Flatten@{cpraw[[j]],If[txtQ,{":>",N[440 2.^(-9/12)*dat[[j,1]]],"Hz"},If[txtQ,{":>",cpraw[[j]]},""]]},TooltipDelay->0.6,TooltipStyle->{Background->Darker[Hue[huex[[j]]/lcrdu],.5],White}]&@With[{j=j},Button[#,s[crd[[j]]]]]&@Line[{coord[[j,1]],coord[[j,1]]+hf/rtioC*crd12[[j]]*{0,1}}],Dashing[None],If[OptionValue[text],#,{}]&@With[{j=j},Button[#,Speak[If[txtQ,StringReplace[#,"#"->",sharp"],#]&@cpraw[[j]]];s[crd[[j]]];]&@Text[Framed[Style[cpraw[[j]],9,FontFamily->"Times New Roman"],RoundingRadius->5],coord[[j,1]]+hf/2/rtioC*crd12[[j]]*{0,1},Background->col]]},{j,lcrd}];
(*origin of 1*)org=Circle[If[txtQ,{3/4,0},{0,0}],.12/rtio];
(*Points*)pnt=Point/@Union[Flatten[Table[If[dat[[i,j]]>mxplt,{},#]&@coord[[i,j]],{i,lcrd},{j,ovrn}],1]][[2;;]];
grh=Graphics[Flatten@{Gray,Thick,If[mn<1<mx+2,org],Thin,Dashed,If[tf,ln2,{}],Dotted,lnN,Dashing[None],Thick,ln,Thin,lncrd,Thick,lnbs,Gray,PointSize[Medium],pnt},ImageSize->1600,Background->col]]


(*Protect information*)Attributes[soil]={ReadProtected};
Clear["Global`*"];
soil::usage="soil[file,t0,d,Patterns]
file is the directory of audio file;
t0 is the starting time;
d is the time duration;
obvious: the obious of peaks, usually from 20 to 60;
temperment: the 12 or other temperments ticks;
colorfunction: the plot color of the frequency components;
color: color of frequency components;
reverse: reverse the left and right audio;
phase: showing phase dots;
mono: conver to mono mode;
algorithm: frequency manipulation algorithm: 1 or 2;";
soil[file_?StringQ,T0_?NumberQ,d_?NumberQ,OptionsPattern[]]:=Module[{aud,baseline,chn,cir4,col,cols,dat,decir,Faud,Fline,freq,freq1,freq1a,freq2,freq2a,freqline,freqline2,freqvol,leftp,leng,lengo,minf,mn,mx,name,nmb,nme,nmecol,oct,phas,phsdt,posx,posy,Rd,rline,s,sprt,t,t0,tend,tleng,tmpN,tmprline,tmprline2,sound,tpcol,tstat,X,\[Omega]},Options[soil]={colorfunction->"TemperatureMap",temperment->12,color->False,reverse->False,phase->False,mono->False,obvious->30,algorithm->2};
col=ColorData[OptionValue[colorfunction]];
(*initialize octave of sound play*)oct=0;
(*import Data*)dat=Import[file,"Sound"];
(*sample rate*)sprt=dat[[1,2]];
(*Audio Channel Number*)chn=If[OptionValue[mono],1,Length@dat[[1,1]]];
(*starting time*)t0=T0;
(*starting position*)tstat=If[#>Length@dat[[1,1,1]],0,#]&@Floor[t0*sprt]+1;
(*time duration*)t=d;
(*duration spans*)tleng=Floor[t*sprt];
(*end position*)tend=If[#>Length@dat[[1,1,1]],Length@dat[[1,1,1]],#]&@(tstat+tleng);
(*temperment ticks*)tmpN=OptionValue[temperment];
(*name of 12*)nme={"C","C#","D","D#","E","F","F#","G","G#","A","A#","B"};
nmecol={0,1,0,1,0,0,1,0,1,0,1,0};
(*get data*)aud=If[OptionValue[reverse],Reverse@#,#]&@dat[[1,1,;;,tstat;;tend]];
(*lengths*)lengo=Length@aud[[1]];
leng=Floor[lengo/2];
(*FFT*)Faud=Fourier/@(If[OptionValue[mono],{Total[#]},#]&@aud);
(*Omegas*)\[Omega]=1.*If[#<2,#,#-1]&/@Range[leng]*sprt/lengo ;
(*music scale*)posy=Floor[Log[2,\[Omega]/(440*2^(-9/12))]];
(*width of graphics*)X=12.*GoldenRatio;
(*position of each freq components*)posx=X*If[#<0,1+#,#]&/@FractionalPart[Log[2,\[Omega]/(440*2^(-9/12))]];
(*extract 2 freqs*)freq=(Log[Abs@Faud[[#,;;leng]]])&/@Range[chn];
(*sound function volume*)freqvol=If[chn>1,(#/Max[#])&/@Transpose[(Abs@Faud[[#,;;leng]])&/@Range[chn]]];
(*extract 2 phase difference*)phas=If[chn>1,Flatten@Differences[-(Arg@Faud[[#,;;leng]])&/@Range[chn]],{}];
phas=Mod[phas,2\[Pi]]/(2\[Pi]);
freq1=freq[[1]];
freq2=freq[[-1]];
(***algorithm 1: old version***)
Switch[OptionValue[algorithm],1,mx=Max[Flatten@{freq1,freq2}];mn=Mean[Flatten@{freq1[[-10;;-1]],freq2[[-10;;-1]]}];
freq1a=(If[#>0,#,0]&/@((freq1-mn)/(mx-mn)))^(OptionValue[obvious]/100+1);
freq2a=(If[#>0,#,0]&/@((freq2-mn)/(mx-mn)))^(OptionValue[obvious]/100+1);,
2,
(***algorithm 2***)
freq1a=(OptionValue[obvious]/100+1)^freq1;
freq2a=(OptionValue[obvious]/100+1)^freq2;
mx=Max[Flatten@{freq1a,freq2a}];
mn=Min[Flatten@{freq1a,freq2a}];
freq1a=freq1a-Min@freq1a;freq1a=freq1a/Max@freq1a;
freq2a=freq2a-Min@freq2a;freq2a=freq2a/Max@freq2a;]

(*Sound function*)s[frequency_,phase_,volume_]:=EmitSound[Play[{volume [[1]]*Sin[frequency*2\[Pi] t],volume [[-1]]*Sin[frequency*2\[Pi] t+phase]},{t,0,.1},SampleRate->sprt,PlayRange->{-1,1}]];
(*UI*)
(*color of Frequency Components*)cols:=If[OptionValue[color]&&(chn!=1),col[(ArcTan[3#]/ArcTan[3]&@(freq1a[[i]]-freq2a[[i]])+1)/2],{}];
(*minimum freq*)minf=-5;
(*draw frequency Components*)freqline=Table[If[posy[[i]]<minf,{},{cols,If[!OptionValue[color]||(chn==1),GrayLevel[1-1/2(freq1a[[i]]+freq2a[[i]])],{}],Line[{{posx[[i]],posy[[i]]},{posx[[i]],posy[[i]]+If[chn==1,.96,.5]freq1a[[i]]}}]}],{i,Length@freq1}];
(*draw frequency Components 2*)freqline2:=Table[If[posy[[i]]<minf,{},{cols,If[!OptionValue[color]||(chn==1),GrayLevel[1-1/2(freq1a[[i]]+freq2a[[i]])],{}],Line[{{posx[[i]],posy[[i]]},{posx[[i]],posy[[i]]-.5freq2a[[i]]}}]}],{i,Length@freq2}];
(*Left place*)leftp=-X/70;
(*baseline*)baseline=Table[If[tmpN==12,With[{i=i},Button[#,oct=i]],#]&@Line[{{leftp,i},{X-leftp,i}}],{i,minf+1,posy[[-1]]}];
(*play sound notes*)sound[x_,j_]:=If[tmpN==12,With[{i=j},Button[x,If[(i+12*oct)<68,EmitSound@Sound@SoundNote[(i+12*oct),.1]]]],x];
(*termperment ticks*)
tmprline=Table[If[OptionValue[phase],#,sound[#,i]]&@Line[{{i*X/tmpN,posy[[-1]]+1},{i*X/tmpN,minf}}],{i,0,tmpN}];
(*music scale number*)nmb=Table[If[tmpN==12,With[{i=i},Button[#,oct=i]],#]&@Text[Framed[RomanNumeral[i+4],FrameStyle->Thick,RoundingRadius->5],{leftp,i},Background->White],{i,minf+1,posy[[-1]]}];
(*name of notes*)name=Table[sound[#,i]&@Text[Framed[If[tmpN==12,nme[[Mod[i,12]+1]],Mod[i,tmpN]],Background->White,RoundingRadius->5,FrameStyle->Thick],{i*X/tmpN,minf}],{i,0,tmpN}];
(*phase dots*)
phsdt:=Table[If[posy[[i]]<minf,{},{Hue[phas[[i]]],StatusArea[#,ToString[N[\[Omega][[i]],2]]<>"Hz || "<>If[phas[[i]]<.5,"R","L"]<>ToString[360*If[phas[[i]]<.5,phas[[i]],1-phas[[i]]]]<>"\[Degree]"]&@With[{i=i},Button[#,s[\[Omega][[i]],phas[[i]]*2\[Pi],freqvol[[i]]];oct=posy[[i]]]]&@Disk[{posx[[i]],posy[[i]]},(ArcTan[3#]/ArcTan[3]&@Abs[If[OptionValue[color],.04(freq1a[[i]]-freq2a[[i]]),0.01(freq1a[[i]]+freq2a[[i]])]])]}],{i,Length@freq1}];
(*Frame Lines*)
(*Left, Right, Top, Bottom Line*)Fline={Line[{{leftp,posy[[-1]]+1},{leftp,minf}}],Line[{{X-leftp,posy[[-1]]+1},{X-leftp,minf}}],Line[{{leftp,posy[[-1]]+1},{X-leftp,posy[[-1]]+1}}],Line[{{leftp,minf},{X-leftp,minf}}]};
(*decoration circles*)Rd=.08;decir={EdgeForm[Thick],Table[{If[nmecol[[Mod[i,12]+1]]==1&&tmpN==12,Gray,LightGray],Disk[{i*X/tmpN,posy[[-1]]+1},.6Rd]},{i,0,tmpN}],Table[Disk[{X-leftp,i},.5Rd],{i,minf+1,posy[[-1]]}],White,Disk[#,Rd]&/@{{leftp,posy[[-1]]+1},{leftp,minf},{X-leftp,posy[[-1]]+1},{X-leftp,minf}}};
(*4circle*)cir4=Button[#,CreatePalette[tpcol,WindowTitle->"Color Reference"]]&@Circle[{X-leftp,0},Rd];
(*color reference*)tpcol:=Graphics[Flatten@{Thickness[.01],Table[{If[OptionValue[phase],Hue,If[OptionValue[color]&&chn!=1,ColorData[OptionValue[colorfunction]],GrayLevel]][\[Theta]/(2\[Pi])],Disk[{-Cos[\[Theta]+\[Pi]/2],Sin[\[Theta]+\[Pi]/2]},.15],Line[{{0,0},{-Cos[\[Theta]+\[Pi]/2],Sin[\[Theta]+\[Pi]/2]}}],If[OptionValue[color]&&chn!=1,ColorData[OptionValue[colorfunction]],GrayLevel][\[Theta]/(2\[Pi])+1/12],Disk[1./GoldenRatio{-Cos[\[Theta]+\[Pi]/2-\[Pi]/12],Sin[\[Theta]+\[Pi]/2-\[Pi]/12]},.15/GoldenRatio],Line[{{0,0},1./GoldenRatio{-Cos[\[Theta]+\[Pi]/2-\[Pi]/12],Sin[\[Theta]+\[Pi]/2-\[Pi]/12]}}]},{\[Theta],\[Pi]/6.,2\[Pi],\[Pi]/6}],Black,EdgeForm[Directive[White,Thickness[.01]]],Button[#,EmitSound[ListPlay[aud,SampleRate->sprt]]]&@Disk[{0,0},.1],If[chn==1,{},{White,Thickness[.006],Line[{{-.1,0},{.1,0}}],Text[Style["\:53f3",FontFamily->"SimHei"],{0,-.05}],Text[Style["\:5de6",FontFamily->"SimHei"],{0,.05}]}]},Background->Black];
(*Draw graph*)Graphics[Flatten@{Thick,LightGray,Dashed,Thickness[Medium],tmprline,Black,Dashing[None],Thick,freqline,If[chn==1,{},freqline2],Black,Fline,cir4,Gray,Thickness[Medium],baseline,If[chn>1&&OptionValue[phase],phsdt,{}],decir,Black,name,nmb},ImageSize->1600]]
