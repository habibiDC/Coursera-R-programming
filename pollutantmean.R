pollutantmean<-function(directory,pollutant,id=1:332){
        #create a list of files from the directory        
        listfile<-list.files(path=directory,pattern = ".csv",full.names = TRUE)
        #create a vector to store the data to be used to calculate the mean        
        values=numeric()
        for (i in id){
                data=read.csv(listfile[i])
                #combine data with selected polutant from each monitor location
                values<-c(values,data[[pollutant]])
        }
        mean(values,na.rm=TRUE)
}
        

              
               