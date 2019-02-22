corr<-function(directory, threshhold=0){
        listfile<-list.files(path=directory,pattern = ".csv",full.names = TRUE)
        #create a vector in store values of correlations
        correlations<-numeric()
        t=threshhold
        #loop to reach each csv file
        for (i in 1:332){
                data=read.csv(listfile[i])
                #test to see if the number of complete rows is greater than the threshhold
                if (sum(complete.cases(data))>t){
                #subset data with complete rows and select colomns nitrate and sulfate
                sub<-subset(data[complete.cases(data),],select=c(nitrate,sulfate))
                #combine the values of orrelations
                correlations<-c(correlations,cor(x=sub$nitrate,y=sub$sulfate))
                }
        }
        return(correlations)
}
