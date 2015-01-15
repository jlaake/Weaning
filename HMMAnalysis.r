library(marked)
detach("package:RMark")

extract.Zc_byarea=function(file="Weaning&SurvivalStudy.accdb",dir="")
{
	xx=require(RODBC,quietly=TRUE)
	connection=odbcConnectAccess2007(file)
#   Fetch initial and resight data tables
	Initial=sqlFetch(connection,"Initial")
	Resights=sqlFetch(connection,"Resights")
#   Exclude first column which is not needed and exclude resights between 10/8/12 and 5/15/2013
	Resights=subset(Resights,subset=as.Date(Sitedate)<=as.Date("2012-10-08") | as.Date(Sitedate)>= as.Date("2013-05-15"), select=names(Resights)[-1])
#   Merge brands and their resights
	merged=merge(Initial,Resights,by.x="ID",by.y="AnimalID",all.x=TRUE)
#   pool all dates and then create occasion partitions
	all.dates=unique(names(table(merged$Sitedate)))
#   occasion dates are the first and last date of a trip or split trip in March and April to get 2 occasions
	occasion.dates=as.Date(c("10-24-2010","10-29-2010","11-29-2010","12-06-2010","1-17-2011",
					"1-25-2011","2-21-2011","3-1-2011","3-14-2011","3-19-2011","3-23-2011","4-15-2011","4-19-2011","4-24-2011",
					"5-23-2011","6-13-2011","6-20-2011","6-25-2011",
					"7-10-2011","8-19-2011","5-1-2012","6-30-2012",
					"7-1-2012","8-10-2012","9-17-2012","10-8-2012","05-15-2013","06-30-2013","08-15-2013"),"%m-%d-%Y")
#   create an occasion field by cutting site dates with occasion dates	
	merged$occasion=cut(as.Date(merged$Sitedate),occasion.dates,include.lowest=TRUE)
#   drop any unused factor levels
	merged=droplevels(merged)
#   create an areas vector of  SMI areas 1: area 1, 2: rest of SMI other than east end, 3: east end and ANI which is area 4.
	areas=ifelse(merged$code%in%c("ACV","EACS","EAC","LNC","LTC","OCV","ONC"),1,ifelse(merged$code%in%c("GFB","ECK","CWD","CWP","CRW"),3,ifelse(merged$code=="ANI",4,2)))
#   create a 3-way table of frequencies (counts) of resights by ID (sea lion), occasion and area
	tt=table(merged$ID,merged$occasion,areas)
#   if any are greater than 0 assign 1 so ones seen multiple times are set to 1
	tt[tt>0]=1
#   create a capture history matrix with ID as rows and occasions as columns and the value is
#   the lowest area in which they were seen. For example, if they were seen in area 1 and 3 during the same occasion
#   it assigns 1.	
	chmat=apply(tt,c(1,2),function(x) {
				z=x*1:4
				if(any(z>0))
					return(min(z[z>0]))
				else
					return(0)
			})
#   the row names of chmat are the ids			
	bID=row.names(chmat)
#   for each row in chmat, paste the elements together to create a capture history
	ch=apply(chmat,1,paste,collapse="")
#   extract needed fields from Initial data frame and store in initial data frame
	initial=subset(Initial,select=names(Initial)[c(2,7,9:13,20:26)])
#   create a dataframe with ID and ch - capture history
	data=data.frame(ID=bID,ch=ch)
#   merge data with initial to grab those fields
	data=merge(data,initial,by.x="ID",by.y="ID")
#   turn ch into a character if it isn't already a character
	data$ch=as.character(data$ch)
#   add on occasions for initial release in July for Paternity pups and in Sept for brand/tag pups
	initial.ch=rep("01",nrow(chmat))
	initial.ch[data$Treatment=="Paternity"]="10"
	data$ch=paste(initial.ch,data$ch,sep="")
#   create fecal time
	data$FecalTime=as.POSIXct( strptime(paste(as.character(data$Date),
							paste(formatC(as.POSIXlt(data$FecalTime)$hour,width=2,flag=0),formatC(as.POSIXlt(data$FecalTime)$min,width=2,flag=0),sep=":")),"%Y-%m-%d %H:%M"))
#   create 0/1 variables: Brand =1 if branded and 0 if not; Tag = 1 if tagged and 0 if not
	Brand=rep(0,nrow(data))
	Brand[substr(data$Treatment,1,5)=="Brand"]=1
	Tag=1-Brand
#   turn Brand into a factor variable brand and Tag into a factor variable tag
	data$brand=factor(Brand)
	data$tag=factor(Tag)
#   create an experiment variable: 1 if in Survival/Weaning study and 0 otherwise 
	data$experiment=0
	data$experiment[data$Study=="SW"]=1
	data$experiment=factor(data$experiment)
#   turn Brander character into factor variable brander and remove Brander (set to NULL)
	data$brander=factor(data$Brander)
	data$Brander=NULL
#   change name of Sex to sex	
	names(data)[names(data)=="Sex"]="sex"
#   create condition index weight/length
	data$CI=data$Weight/data$Length
#   Do Suckle capture history as well going through the same steps as above
#   extract resights of "S" or "M" behavior
	suckle=subset(Resights,subset=behavior%in%c("S","M"))
	merged=merge(Initial,suckle,by.x="ID",by.y="AnimalID",all.x=TRUE)
#   pool all dates and then create occasion partitions
	merged$occasion=cut(as.Date(merged$Sitedate),occasion.dates)
	merged=droplevels(merged)
	areas=ifelse(merged$code%in%c("ACV","EACS","EAC","LNC","LTC","OCV","ONC"),1,ifelse(merged$code%in%c("GFB","ECK","CWD","CWP","CRW"),3,ifelse(merged$code=="ANI",4,2)))
	tt=table(merged$ID,merged$occasion,areas)
	tt[tt>0]=1
	suckle_chmat=apply(tt,c(1,2),function(x) {
				z=x*1:4
				if(any(z>0))
					return(min(z[z>0]))
				else
					return(0)
			})
#   Because suckle_chmat only includes suckling observations and there were no suckling (S or M) observations, the
#   number of columns in suckle_chmat is less than chmat, so the following pads with columns containing 0 at the end
	suckle_chmat=cbind(suckle_chmat,matrix(0,ncol=ncol(chmat)-ncol(suckle_chmat),nrow=nrow(suckle_chmat)))
#   Now create the area-S or area-U structure needed for the state uncertainty model with areas specified
    area_chmat=matrix(0,nrow=nrow(chmat),ncol=ncol(chmat))
#   loop over rows (i) and columns (j)
	for(i in 1:nrow(chmat))
	for(j in 1:ncol(chmat))
	{
#       if for some reason suckle_chmat has a non-zero value and chmat and suckle_chmat disagree
#       set it to suckle value. This over-rides so as to maintain the observed suckling.  In the data
#       this will only occur when a pup was seen in areas 1 and 2 on the same occasion and it was seen
#       suckling in area 2. 
		if(suckle_chmat[i,j]>0&chmat[i,j]!=suckle_chmat[i,j])chmat[i,j]=suckle_chmat[i,j]
#       if it was seen suckling then it pastes the area with "S"
		if(chmat[i,j]>0)
#           if it was seen suckling then it pastes the area with "S"
			if(suckle_chmat[i,j]>0)
				area_chmat[i,j]=paste(chmat[i,j],"S",sep="")
		    else
#               otherwise if it was seen but not seen suckling then it gets U
				area_chmat[i,j]=paste(chmat[i,j],"U",sep="")
	}		
#   now paste the area_chmat values by row	
	ch=apply(area_chmat,1,paste,collapse=",")
#   assign 1S for the first occasion when it was released at time of branding
	ch=paste("1S",ch,sep=",")
#   assign ch to suckle.ch
	data$suckle.ch=ch	
#   get rid of any records that are all 0 - should not be any
	data=data[as.numeric(data$ch)>0,]
#   assign times (dates) which are used to create time.intervals between occasions.  The approximate mid-point dates are used because sampling occurs
#   over several days in each occasion.
	times= as.Date(c("2010-7-31","2010-09-26","2010-10-26","2010-12-02","2011-01-21","2011-02-25","2011-03-16","2011-03-20","2011-04-17","2011-04-20",
					"2011-06-03","2011-06-16","2011-06-22","2011-07-01","2011-07-19","2012-06-10","2012-07-20","2012-09-27","2013-07-01"))
#   check to make sure the number of times and the number of occasions agree; this ignores the July-Sept period.
	if((length(times)-2)!=ncol(chmat))stop("times need to be adjusted")
#   close the connection to the access database
	odbcClose(connection)
#   return a list with the dataframe named data and the vector of times.
	return(list(data=data,times=times))
}


# This creates the data used for the analysis.  Those data are in Weaningdata.xlsx
zc=extract.Zc_byarea()
zc$data$initial.age=0.238356
# Extract branded sea lions which excludes tagged only
zc$data=zc$data[zc$data$brand==1,]
zc$data$CI=zc$data$Weight/(zc$data$Length/100)
# Compute sex-specific mean weights
mean.wts=tapply(zc$data$Weight,zc$data$sex,mean)
zc$data$Weight[zc$data$sex=="F"]=zc$data$Weight[zc$data$sex=="F"]-mean.wts[1]
zc$data$Weight[zc$data$sex=="M"]=zc$data$Weight[zc$data$sex=="M"]-mean.wts[2]
# create a weight factor variable which are skinny (< -2), normal (-2 to +2), fat (>2)
zc$data$Wt=cut(zc$data$Weight,c(-15,-1.5,1.5,15))
# Use suckle.ch for this analysis
zc$data$ch=zc$data$suckle.ch
zc$data$brandnum=zc$data$ID
zcdata=subset(zc$data,subset=brand==1,select=c("ch","sex","Weight","Length","Wt","initial.age","brandnum"))
times=diff(zc$times)[-1]/365

# modify ch such that observations at age 2 or older are considered weaned
chmat=do.call("rbind",strsplit(zcdata$ch,","))
chmat[,15:18]=sub("U","W",chmat[,15:18])
zcdata$ch=apply(chmat,1,paste,collapse=",")

# save data to put in excel spreadsheet
chmat=do.call("rbind",strsplit(zcdata$ch,","))
write.table(cbind(subset(zcdata,select=c("brandnum","sex","Weight","Wt")),chmat),file="Weaningdata.txt")

# with marked packge process data with Multistate model with state uncertainty and independent area-state structure
zcp=process.data(zcdata,model="HMMu2iMSCJS",strata.labels=list(area=c(1,2,3,4),states=c("S","W")),time.intervals=as.vector(times),begin.time=.238356)
# make design data
zcddl=make.design.data(zcp)

# Modify design data
# S ddl
zcddl$S$fix=NA
# fix survival to 1 for closed periods
zcddl$S$fix[zcddl$S$occ%in%c(6,8)]=1
# create variables used in models
zcddl$S$ageclass=cut(zcddl$S$occ,breaks=c(0,13,15,19),include.lowest=T,labels=c("Pup","Yearling","2yr"))
zcddl$S$session=cut(zcddl$S$occ,breaks=c(0,3,13,15,19),labels=c("EarlypupHW","LatePup","Yearling","2yr"))
zcddl$S$nonpup=ifelse(zcddl$S$ageclass!="Pup",1,0)
zcddl$S$yearling=ifelse(zcddl$S$ageclass!="Yearling",1,0)
zcddl$S$pup=1-zcddl$S$nonpup
zcddl$S$W=ifelse(zcddl$S$state=="W",1,0)
zcddl$S$ANI=ifelse(zcddl$S$area=="4",1,0)

#Psi ddl
# transition from weaned to suckling is not possible
zcddl$Psi$fix[zcddl$Psi$state=="W"&zcddl$Psi$tostate=="S"]=0
# transition from suckling to weaned is prevented duing closed periods
zcddl$Psi$fix[zcddl$Psi$state=="S"&zcddl$Psi$tostate=="W"&zcddl$Psi$occ%in%c(6,8)]=0
# starting at occasion 14 - age 1 to 2; all are weaned
zcddl$Psi$fix[zcddl$Psi$state=="S"&zcddl$Psi$tostate=="W"&zcddl$Psi$occ==14]=1
zcddl$Psi$fix[zcddl$Psi$state=="S"&zcddl$Psi$tostate=="S"&zcddl$Psi$occ==14]=0
zcddl$Psi$fix[zcddl$Psi$state=="S"&zcddl$Psi$tostate=="W"&zcddl$Psi$occ%in%15:18]=0
# add time bins and weight and male variables for Psi
zcddl$Psi$tbin=cut(zcddl$Psi$occ,c(0,2,4,5:9,14,18),right=TRUE)
zcddl$Psi$early=ifelse(zcddl$Psi$occ%in%1:4,1,0)
zcddl$Psi$heavy=ifelse(as.numeric(zcddl$Psi$Wt)==3,1,0)
zcddl$Psi$male=ifelse(zcddl$Psi$sex=="M",1,0)

#alpha ddl

# do not allow movement to ANI prior to occasion 6
zcddl$alpha$fix[zcddl$alpha$area!="4"&zcddl$alpha$toarea=="4"&zcddl$alpha$occ%in%1:5]=0
# do not allow movement to area 3 prior to occasion 4
zcddl$alpha$fix[zcddl$alpha$area!="3"&zcddl$alpha$toarea=="3"&zcddl$alpha$occ%in%1:3]=0
# create variables for models of movement
zcddl$alpha$tbin=cut(zcddl$alpha$occ,c(0,2,4,20))
zcddl$alpha$nonpup=ifelse(zcddl$alpha$occ>=13,1,0)
zcddl$alpha$to4=ifelse(zcddl$alpha$toarea=="4",1,0)
zcddl$alpha$male=ifelse(zcddl$alpha$sex=="M",1,0)

#delta ddl
# no errors in state for occasions 15-18 because all are considered weaned
zcddl$delta$fix[zcddl$delta$occ%in%15:18]=0.9999999999
# probability of saying it is weaned for occasions 1-14 is 0
zcddl$delta$fix[zcddl$delta$state=="W"&zcddl$delta$occ%in%1:14]=0
# create variable for second occasion when forgot to record suckle behavior
zcddl$delta$t1=ifelse(zcddl$delta$occ==2,1,0)

# p ddl - create variables for models
zcddl$p$W=ifelse(zcddl$p$state=="W",1,0)
zcddl$p$session=cut(zcddl$p$occ,breaks=c(1,2,3,4,5,7,9,14,17,19))
zcddl$p$SMI=ifelse(!zcddl$p$area==4,1,0)
zcddl$p$pup=ifelse(zcddl$p$occ<=12,1,0)
# set p=0 for ANI and East End for occasions with no sampling effort
zcddl$p$fix=NA
zcddl$p$fix[zcddl$p$area==4&zcddl$p$occ%in%c(2:9,17)]=0
zcddl$p$fix[zcddl$p$area==3 & zcddl$p$occ==17]=0


# run models in 3 batches: one for each of the 3 models created for resight probability
do_wean=function()
{
p.1=list(formula=~W+area)		
#p.2=list(formula=~W+time+area)		
#p.3=list(formula=~-1+W+time:area)		

Psi.1=list(formula=~Time)				 
Psi.2=list(formula=~Time+Wt)				 
Psi.3=list(formula=~tbin)
Psi.4=list(formula=~tbin+Wt)
Psi.5=list(formula=~tbin+early:heavy)
Psi.6=list(formula=~tbin+early:male)

alpha.1=list(formula=~-1+male:to4+area:toarea)
alpha.2=list(formula=~-1+area:toarea+nonpup:area:toarea)
alpha.3=list(formula=~-1+area:toarea+nonpup:area:toarea+male:to4)

delta.1=list(formula=~t1+area)
delta.2=list(formula=~time+area) 

S.1=list(formula=~ageclass)
S.2=list(formula=~ageclass+sex)
S.3=list(formula=~pup:Wt)
S.4=list(formula=~pup:Wt+ANI)
S.5=list(formula=~pup:Wt+yearling:Wt)
S.6=list(formula=~-1 + pup:Wt+nonpup:sex)

cml=create.model.list(c("S","Psi","alpha","delta","p"))
return(crm.wrapper(cml,data=zcp,ddl=zcddl,external=FALSE,hessian=FALSE))
}

p1.wean.models=do.wean()

do_wean=function()
{
#p.1=list(formula=~W+area)		
p.2=list(formula=~W+time+area)		
#p.3=list(formula=~-1+W+time:area)		

Psi.1=list(formula=~Time)				 
Psi.2=list(formula=~Time+Wt)				 
Psi.3=list(formula=~tbin)
Psi.4=list(formula=~tbin+Wt)
Psi.5=list(formula=~tbin+early:heavy)
Psi.6=list(formula=~tbin+early:male)

alpha.1=list(formula=~-1+male:to4+area:toarea)
alpha.2=list(formula=~-1+area:toarea+nonpup:area:toarea)
alpha.3=list(formula=~-1+area:toarea+nonpup:area:toarea+male:to4)

delta.1=list(formula=~t1+area)
delta.2=list(formula=~time+area) 

S.1=list(formula=~ageclass)
S.2=list(formula=~ageclass+sex)
S.3=list(formula=~pup:Wt)
S.4=list(formula=~pup:Wt+ANI)
S.5=list(formula=~pup:Wt+yearling:Wt)
S.6=list(formula=~-1 + pup:Wt+nonpup:sex)

cml=create.model.list(c("S","Psi","alpha","delta","p"))
return(crm.wrapper(cml,data=zcp,ddl=zcddl,external=FALSE,hessian=FALSE))
}


p2.wean.models=do.wean()

do_wean=function()
{
#p.2=list(formula=~W+time+area)		
p.3=list(formula=~-1+W+time:area)		

Psi.1=list(formula=~Time)				 
Psi.2=list(formula=~Time+Wt)				 
Psi.3=list(formula=~tbin)
Psi.4=list(formula=~tbin+Wt)
Psi.5=list(formula=~tbin+early:heavy)
Psi.6=list(formula=~tbin+early:male)

alpha.1=list(formula=~-1+male:to4+area:toarea)
alpha.2=list(formula=~-1+area:toarea+nonpup:area:toarea)
alpha.3=list(formula=~-1+area:toarea+nonpup:area:toarea+male:to4)

delta.1=list(formula=~t1+area)
delta.2=list(formula=~time+area) 

S.1=list(formula=~ageclass)
S.2=list(formula=~ageclass+sex)
S.3=list(formula=~pup:Wt)
S.4=list(formula=~pup:Wt+ANI)
S.5=list(formula=~pup:Wt+yearling:Wt)
S.6=list(formula=~-1 + pup:Wt+nonpup:sex)

cml=create.model.list(c("S","Psi","alpha","delta","p"))
return(crm.wrapper(cml,data=zcp,ddl=zcddl,external=FALSE,hessian=FALSE))
}

p3.wean.models=do.wean()

library(marked)

do_wean=function()
{
p.1=list(formula=~W+area)		

Psi.3=list(formula=~tbin)
alpha.3=list(formula=~-1+area:toarea+nonpup:area:toarea+male:to4)


delta.2=list(formula=~time+area) 

S.3=list(formula=~pup:Wt)


cml=create.model.list(c("S","Psi","alpha","delta","p"))
return(crm.wrapper(cml,data=zcp,ddl=zcddl,external=FALSE,hessian=FALSE,debug=FALSE,method=c("nlminb","BFGS"),save.matrices=FALSE))
}

initial=do_wean()


