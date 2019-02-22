complete<-function(directory,id=1:332){
        listfile<-list.files(path=directory,pattern = ".csv",full.names = TRUE)
        #create a vector for data to store the nobs values 
        nobs<-numeric()
        for(i in id){
                data=read.csv(listfile[i])
                #get the number of complete rows in data
                ncomplete=sum(complete.cases(data))
                #combine nobs from each csv
                nobs<-c(nobs,ncomplete)
        }
        #create a data frame that contains id and nobs
        data.frame(id,nobs)
}