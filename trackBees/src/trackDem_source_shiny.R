# Author: Alvaro Vega Hidalgo | Wcislo Lab | Smithsonian Tropical Research Institute | alvarovegahd@gmail.com

# libraries ----

#library(trackdem) # instead run local code with modifications
# library(tictoc)
library(tidyverse)
library(kriging)
library(trajr)
library(ggpubr)
library(progress)
library(mapmate)
library(png)


# master Function shiny ----
trackSingleParticle_Master<-function(dir_cutCoordsTable,
                                     stTime_cut="00:00:04",
                                     endTime_cut="00:13:59",
                                     framesPerIteration_ToIterations=100,
                                     area_thr_toAnimation=1000,
                                     eraseTmpFiles=T){
  st_total <- print(Sys.time())
  # START debugg -----
  # dir_cutCoordsTable<-"/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny/thumbnails/crop_data.csv"
  # stTime_cut <- "00:00:04"
  # endTime_cut <- "00:13:59"
  # framesPerIteration_ToIterations=100
  # area_thr_toAnimation=1000
  # # END debugg -----
  
  cutCoordsTable<-read.table(dir_cutCoordsTable,header = T,stringsAsFactors = F)
  
  # Make sure we get just the last crop of each image in cas the user messed up
  cutCoordsTable<-cutCoordsTable[!duplicated(cutCoordsTable$fullDirVideo),]
  dirRoot <- dirname(cutCoordsTable$fullDirVideo[1])
  resultsDir <- paste0(dirRoot,"/results")
  if(!dir.exists(resultsDir)){
    dir.create(resultsDir)
  }
  
  # Run cutVideo as many times there is video files
  for(i in cutCoordsTable$n){
    dirVideo_tocut_tmp <- cutCoordsTable$fullDirVideo[i]
    
    # debugg
    #stTime_cut<-"00:00:03"
    #endTime_cut<-"00:00:20"
    
    cutVideo(start_time = stTime_cut,end_time = endTime_cut,
             dirVideo_tocut = dirVideo_tocut_tmp,
             x1 = cutCoordsTable$x_min[i],
             x2 = cutCoordsTable$x_max[i],
             y1 = cutCoordsTable$y_min[i],
             y2 = cutCoordsTable$y_max[i])
    
    Sys.sleep(0.5)
    
    # Then, create Image sequences of that i Video
    dirVideo_cut_toImgSeq_tmp <- paste0(dirRoot,"/cutVideoTmp/",basename(dirVideo_tocut_tmp),"_TMP_imagecut.mp4")
    
    createImgSeq(
      dir_videoToImgSeq = dirVideo_cut_toImgSeq_tmp,
      dirImgSeqOut = dirRoot
    )
    
    # Create results directory for the ivideo
    resultsDir_ivideo <- paste0(resultsDir,"/Results_",basename(dirVideo_tocut_tmp))
    if(!dir.exists(resultsDir_ivideo)){
      dir.create(resultsDir_ivideo)
    }
    
    # Then track the video
    dir_framesVideo <- paste0(dirRoot,"/imgSeqTmp/",basename(dirVideo_cut_toImgSeq_tmp))
    rawTracked_tmp <- iterate_partIden(set_Thr = -0.01,
                                       set_PixelRange = c(0,10000),
                                       framesInputDir = dir_framesVideo,
                                       framesPerIteration = framesPerIteration_ToIterations,
                                       outputDir = resultsDir_ivideo,
                                       subsetOfIterations = 0)
    # Read tracked Video
    createAnimations_fromIterations(
      track_data_from_identifyParticles = rawTracked_tmp,
      area_thr = area_thr_toAnimation,
      imgFrames_dir = dir_framesVideo,
      output_dir_framesAnimation = resultsDir_ivideo,
      framesPerIteration = 100,
      prefix_imgs_files = paste0(basename(dirVideo_tocut_tmp),"_TMP_imagecut.mp4_img_"),
      fps_of_videoToimgs = 5,
      secs_of_delay_atStartOfVideo = 3,
      set_dpi = 25,
      n_zeroes = 4,createPlots = T,createVideo = T,errase_imgs = F,
      out_videoname_file = "animation")
    
    if(eraseTmpFiles){
      unlink(paste0(dirRoot,"/cutVideoTmp/",
                    basename(dirVideo_tocut_tmp),"_TMP_imagecut.mp4"),recursive = T)
      file.rename(paste0(dirRoot,"/imgSeqTmp/",
                         basename(dirVideo_tocut_tmp),"_TMP_imagecut.mp4/TrackedVideo.csv"),
                  paste0(dirRoot,"/results/Results_",
                         basename(dirVideo_tocut_tmp),"/TrackedVideo.csv")
      )
      file.rename(paste0(dirRoot,"/imgSeqTmp/",
                         basename(dirVideo_tocut_tmp),"_TMP_imagecut.mp4/Log_Tracking.log"),
                  paste0(dirRoot,"/results/Results_",
                         basename(dirVideo_tocut_tmp),"/Log_Tracking.log")
      )
      Sys.sleep(0.5)
      unlink(paste0(dirRoot,"/imgSeqTmp/",basename(dirVideo_tocut_tmp),"_TMP_imagecut.mp4"),recursive = T)
      system(paste0("rm ",dirRoot,"/results/Results_",basename(dirVideo_tocut_tmp),"/*.png"))
    }
  }
  unlink(paste0(dirRoot,"/imgSeqTmp"),recursive = T)
  unlink(paste0(dirRoot,"/cutVideoTmp"),recursive = T)
  et_total<-print(Sys.time())
  print("Analyzed all videos in :")
  #print(et_total-st_total)
}

# createImSeq ----
createImgSeq <- function(
  dir_videoToImgSeq=NULL,
  dirImgSeqOut=NULL,
  fps=5
){
  # debugg START ----
  # dir_videoToImgSeq="/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny/2.mp4"
  # fps=5
  
  # debugg END ----
  dirImgSeqOut_tmpImgs <- paste0(dirImgSeqOut,"/imgSeqTmp","/",basename(dir_videoToImgSeq))
  if(!dir.exists(dirname(dirImgSeqOut_tmpImgs))){
    dir.create(dirname(dirImgSeqOut_tmpImgs))
  }
  if(!dir.exists(dirImgSeqOut_tmpImgs)){
    dir.create(dirImgSeqOut_tmpImgs)
  }
  system(paste0("ffmpeg -y -i ",dir_videoToImgSeq," -r ",as.integer(fps)," ",dirImgSeqOut_tmpImgs,"/",basename(dir_videoToImgSeq),"_img_%04d.png"))
}

# cutVideo ----

cutVideo<-function(
  # Saves videos cut at the dimensions specified
  start_time="00:00:04",
  end_time="00:13:59",
  dirVideo_tocut=NULL,
  x1,
  x2,
  y1,
  y2
){
  #debugg START ----
  # dirVideo_tocut="/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny/5_bee_14min_imagecut.mp4"
  #  dirVideo_tocut<-"/home/alvaro/Documents/STRI_Bees/WeTransferVideosBees/Alvaro_in_nest_training_videos/converted_to_mp4/2018-03-14_12.00.25.mp4"
  # x1 = 20
  # x2 = 100
  # y1 = 20
  # y2 = 100
  # start_time = "00:00:04"
  # end_time = "00:13:59"

  #debugg END ----
  #create tmp directory if does not exist
  if(!dir.exists(paste0(dirname(dirVideo_tocut),"/cutVideoTmp"))){
    dir.create(paste0(dirname(dirVideo_tocut),"/cutVideoTmp"))
  }
  
  system(paste0("ffmpeg -y"," -i ",dirVideo_tocut," -ss ",start_time," -to ",end_time,
                " -filter:v \"crop=",x2,"-",x1,":",y2,"-",y1,":",x1,":",y1,"\" ", dirname(dirVideo_tocut),"/cutVideoTmp/",basename(dirVideo_tocut),"_TMP_imagecut.mp4"))
}



# createAnimations_fromiterations ----

createAnimations_fromIterations <- function(track_data_from_identifyParticles, # track_data_from_identifyParticles is a dataframe with the columns "x","y","patchID","area","frame","n_iteration"
                                            area_thr=1000, # Threshold of area to plot. 5000 is recommended for Megalopta videos
                                            imgFrames_dir, # Dir of frames of video sent to be analyzed
                                            output_dir_framesAnimation, # Dir where frames for the animation will be exported
                                            framesPerIteration = 100, # How many frames were done per iteration in the iterate_partIden part
                                            prefix_imgs_files = "img_", # prefix pattern before the string "fournumbers.png" when fournumbers are four numbers indicating the frame; 0001,3423,etc
                                            fps_of_videoToimgs = 5, # fps at which video was transformed to imgSequence
                                            secs_of_delay_atStartOfVideo = 3, # -ss parameter at ffmpeg when creating the imageSequence. There is a delay of one second, so if -ss is set to 4 I recommend secs_of_delay_atStartOfVideo=3 
                                            set_dpi = 25,
                                            n_zeroes = 4, # Amount of zeroes on each image. For frames< 9999, 4 zeroes can be used. For more zeroes, add more.#Should change to 6 maybe...
                                            createPlots = T,
                                            createVideo = T,
                                            errase_imgs = T,
                                            out_videoname_file = "animation"
){
  # debugg START----
  
  # track_data_from_identifyParticles <- rawTracked_bee14min
  # i = 1
  # f = 1
  # area_thr = 5000
  # imgFrames_dir <- "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/ImageSequence/bee_14min"
  # framesPerIteration = 100
  # prefix_imgs_files <- "bee_14min_"
  # fps_of_videoToimgs = 5
  # secs_of_delay_atStartOfVideo = 3
  # output_dir_framesAnimation ="docs/Animations/animation_iterations_bee_14min/"
  
  # debugg END----
  
  if(createPlots){
    
    #progress bar
    st<-Sys.time()
    pb <- progress_bar$new(
      format = "  saving imgs [:bar] :percent eta: :eta",
      total = max(track_data_from_identifyParticles$n_iteration), clear = FALSE, width= 100)
    cat("\n")
    # The function will iterate over n_iteration and frame
    for(i in sort(unique(track_data_from_identifyParticles$n_iteration))) {
      
      track_data_from_identifyParticles_n_iterationTmp <- track_data_from_identifyParticles[track_data_from_identifyParticles$n_iteration==i,]
      
      # track_data_from_identifyParticles_n_iterationTmp <- filter(track_data_from_identifyParticles,n_iteration==i)
      
      for(f in sort(unique(track_data_from_identifyParticles_n_iterationTmp$frame))) {
        # Per iteration there is just "framesPerIteration" number of frames (ranging from 1:100 usually)
        # This will be used only to calculate the seconds of the video
        frame_realNumber <- (i-1)*framesPerIteration+f
        
        # Calculate time of frame
        min_dec_ofFrame <- frame_realNumber/fps_of_videoToimgs/60
        min_ofFrame <- as.integer(min_dec_ofFrame)
        sec_ofFrame <- round(min_dec_ofFrame%%1 * 60 + secs_of_delay_atStartOfVideo,1)
        
        img_dir_toBackground <- paste0(prefix_imgs_files,formatC(frame_realNumber,width=n_zeroes,flag="0"),".png")
        
        background_tmp <- readPNG(paste0(imgFrames_dir,"/",img_dir_toBackground))
        
        dfPlot_tmp <- track_data_from_identifyParticles_n_iterationTmp[track_data_from_identifyParticles_n_iterationTmp$area>area_thr & track_data_from_identifyParticles_n_iterationTmp$n_iteration==i & track_data_from_identifyParticles_n_iterationTmp$frame==f,]
        # dfPlot_tmp <- filter(track_data_from_identifyParticles,area > area_thr, n_iteration==i, frame==f) # DO_FASTER: change syntaxis to use r-base [] operator
        
        # I should ADD HERE LOGICAL EVALUATIONS OF: IF NO MOVEMENT, IT IS IN THE SAME SPOT. Add color by this.
        
        # Adding plot rbase START ----
        img_width <- dim(background_tmp)[2]
        img_height <- dim(background_tmp)[1]
        
        # Set image to be saved
        png(filename = paste0(output_dir_framesAnimation,"/img_", 
                              formatC(frame_realNumber,width=n_zeroes,flag="0") ,".png"),
            width = img_width, height = img_height,
            res = set_dpi) # ADD: should change width to number of digits of max frame at some point
        
        #Set up the plot area
        plot(1:2, type='n', main="Tracking movement | Alvaro Vega | alvarovegahd@gmail.com | Wcislo Lab", xlab=" ", ylab=" ",
             xlim = c(0,img_width),
             ylim = c(0,img_height))
        
        #Get the plot information so the image will fill the plot box, and draw it # taken from https://stackoverflow.com/questions/12918367/how-to-plot-with-a-png-as-background
        rasterImage(background_tmp,
                    xleft = 0, 
                    ybottom = 0,
                    xright =  img_width,
                    ytop =  img_height)
        points(dfPlot_tmp$x,(img_height-dfPlot_tmp$y),cex=5,lwd=7)
        text(45,15,
             labels = paste0("min ",min_ofFrame,":",sec_ofFrame),col="darkred",cex = 2)
        
        text(img_width-45,15,
             labels = paste0("frame ",frame_realNumber),col="darkred",cex=2)
        dev.off()
      }
      # IT WOULD BE COOL TO ADD HERE A PART FOR WHEN THERE IS AN INFERRED PARTICLE
      # PAINT THE POINT OF ANOTHER COLOR
      # Adding plot rbase END ----
      # if(sum(dfPlot_tmp$area>area_thr)){} # A way of shortening the time of computation is 
      # avoiding plots without particles
      # g_tmp <- ggplot(dfPlot_tmp, aes(x,dim(background)[1]-y)) +
      #   background_image(background_tmp) +
      #   geom_point(shape=21,size=10,stroke=3) +
      #   geom_line() +
      #   xlim(0,dim(background)[2]) +
      #   ylim(0,dim(background)[1]) +
      #   annotate("text", x=10, y=5, label= paste0("frame ",frame_realNumber),color="darkred") +
      #   annotate("text", x=dim(background)[2]-10, y=5, label= paste0(min_ofFrame,":",sec_ofFrame),color="darkred") +
      #   ylab("") + xlab("")
      # print(g_tmp)
      #Sys.sleep(time = 1)
      # ggsave(filename = paste0(output_dir_framesAnimation,img_dir_toBackground),
      #        plot = g_tmp,height = 1.06,
      #        width = 6.78,dpi=set_dpi)
    }
    # Progress Bar
    pb$tick()
  }
  et <-Sys.time()-st
  print(paste0("Total duration of img creation in minutes: \n"))
  print(et)
  
  if(createVideo){
    print("Creating animation in video")
    # Create video
    patternNameOfFile <- paste0(output_dir_framesAnimation,"img_%04d.png") # should be a local directory or subdirectory, not global; starting from getwd()    
    out <- paste0(output_dir_framesAnimation,"/",out_videoname_file,".mp4")
    
    # 10 fps gif
    # ffmpeg(pattern = patternNameOfFile, output = out, rate = 100,overwrite = T,size = paste0(img_width,"x",img_height))
    system(paste0("ffmpeg -y -r 100 -i ",output_dir_framesAnimation,"/img_%04d.png"," ",out))
    # Same as above. Set overwrite=TRUE if output exists.
    # ffmpeg(pattern = patternNameOfFile, output = out, delay = 1/5, overwrite = TRUE,fps.out = 1)
  }
  if(errase_imgs){#errase all imgs after creating video
    print("Erasing frame images")
    junk <- dir(path=output_dir_framesAnimation, pattern=".png") # ?dir
    file.remove(paste0(output_dir_framesAnimation,junk)) # ?file.remove
  }
}

# iterate_partIden ----

iterate_partIden <- function(set_Thr = -0.01, # thr parameter in trackdem::identifyParticles()
                             set_PixelRange = c(0,10000), # passed to trackdem::identifyParticles()
                             framesInputDir = getwd(), # directory containing the frames of the video sequentially numbered as trackdem needs them
                             framesPerIteration = 100, # Number of frames analyzed per iteration. 100 can be handled by most laptops (at least dual core,4gb ram,550gb HDD. I estimate this; haven't actually tried this)
                             outputDir = getwd(), # output directory to save log, csv and plots if any
                             subsetOfIterations = 0 # An integer. To test the function, a subset of iterations (like just two iterations) is recommended to run first
){
  # Debugging space SHOULD BE DELETED | START----
  
  # setwd("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj")
  # source("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/trackDem_Alvaro_source.R")
  # framesInputDir_ToIterations <- "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/ImageSequence/bee_14min"
  # outputDir_ToIterations <- "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/docs/iterations_bee_14min"
  # 
  # dir(outputDir_ToIterations)
  # dir(framesInputDir_ToIterations)
  # 
  # set_Thr = -0.01
  # set_PixelRange = c(0,10000)
  # framesInputDir = framesInputDir_ToIterations
  # framesPerIteration = 100
  # outputDir = outputDir_ToIterations
  # subsetOfIterations = 2
  # i<-i
  # 
  # Debugging space | END ----
  
  # Will save a csv file with the tracking info and the log file in the directory specified
  # Define threshold for particle identification
  
  # Import images
  imgFiles <- dir(framesInputDir) #FROM_USER
  seqNumImgs <- 1:length(imgFiles)
  
  # Divides the interval seqNumImgs in intervals of framesPerIteration length
  permutInts <- split(seqNumImgs, ceiling(seq_along(seqNumImgs)/framesPerIteration))
  
  # Subsets iteration intervals if that was asked by the user
  if(subsetOfIterations!=0){ permutInts<-permutInts[1:subsetOfIterations] }
  
  cat(paste0("Number of iterations to perform:\n",length(permutInts),"\nOf ",framesPerIteration," frames each\n"))
  
  # Progress bar
  pb <- progress_bar$new(
    format = "  Iterating: particle identification [:bar] :percent eta: :eta",
    total = length(permutInts), clear = FALSE, width= 100)
  cat("\n")
  dateTimeOfRun <- Sys.time()
  for(i in 1:length(permutInts)){
    
    # Obtain tmp values
    allFullImages_tmp <- loadImages(dirPictures=framesInputDir,nImages=as.integer(unlist(permutInts[i])))
    
    cat("\nallFullImages_tmp DONE\n") #debugg
    
    stillBack_tmp <- createBackground(allFullImages_tmp,method='mean')
    
    cat("\nstillBack_tmp DONE\n") #debgg
    
    allImages_tmp <- subtractBackground_in_iterator(bg=stillBack_tmp,colorimages = allFullImages_tmp) # I HAD TO CHANGE THIS FUNCTION BECAUSE BEFORE IT TRIED TO GET IT FROM THE GLOBAL ENVIRONMENT
    
    #THIS WAS THE ERROR MESSAGE:
    # Error in get(attributes(bg)$originalImages, envir = .GlobalEnv) : 
    #object 'allFullImages_tmp' not found 
    
    cat("\nallImages_tmp DONE\n") #debgg
    
    partIdenBee_tmp <- identifyParticles_in_iterator(sbg=allImages_tmp,
                                                     pixelRange=set_PixelRange,# just moving objects between that range of pixel length
                                                     select='dark',
                                                     autoThres=FALSE,threshold=set_Thr,
                                                     colorimages = allFullImages_tmp)
    cat("\npartIdenBee_tmp DONE\n") #debgg
    
    summ_partIdenBee_tmp <- summary(partIdenBee_tmp)
    size_partIdenBee_tmp <- format(object.size(partIdenBee_tmp),units="auto")
    
    # In first iteration, create log file and rawTrack_all dataframe
    if(i==1){
      rawTrack_all <- data.frame(partIdenBee_tmp$x,partIdenBee_tmp$y,
                                 partIdenBee_tmp$patchID,partIdenBee_tmp$area,
                                 partIdenBee_tmp$frame,n_iteration=i)
      cat(paste0("Behavioral ecology of Megalopta: videotracking.\nAuthor: alvaro vega hidalgo alvarovegahd@gmail.com \nDone on date: ",
                 Sys.time(),"\nIteration_",i,"\n",
                 "summ_partIdenBee_tmp\n", summ_partIdenBee_tmp,"\n\n",
                 "size_partIdenBee_tmp\n",size_partIdenBee_tmp,"\n\n\n"),
          file = paste0(outputDir,"/Log_Tracking.log"))
    } else {
      #Append to rawTrack
      rawTrack_tmp <- data.frame(partIdenBee_tmp$x,partIdenBee_tmp$y,
                                 partIdenBee_tmp$patchID,partIdenBee_tmp$area,
                                 partIdenBee_tmp$frame,n_iteration=i)
      rawTrack_all <- rbind(rawTrack_tmp,rawTrack_all)
      
      # Append to Log
      cat(paste(Sys.time(),"\nIteration_",i,
                "summ_partIdenBee_tmp\n", summ_partIdenBee_tmp,"\n\n",
                "size_partIdenBee_tmp\n",size_partIdenBee_tmp,"\n\n\n"),
          file = paste0(outputDir,"/Log_Tracking.log"),append = T)
      
    }
    
    # Remove objects
    rm(list = c("allFullImages_tmp", "stillBack_tmp", "allImages_tmp",
                "partIdenBee_tmp"))
    gc()
    cat(paste0("Done with iteration ",i))
    
    # Progress Bar
    pb$tick()
    
  }
  colnames(rawTrack_all) <- c("x","y",
                              "patchID","area",
                              "frame","n_iteration")
  write.csv(x = rawTrack_all,file = paste0(outputDir,"/TrackedVideo.csv"))
  return(rawTrack_all)
}
# alvaro_createImgsForAnimation----

alvaro_createImgsForAnimation <- function(identifiedParticles=NULL,set_incThres=20,out_path_images="imgs_for_animation",dpi=800,set_imgFiles){
  # Creates Images to animate afterwards usando ffmpeg
  # Example of following ffmpeg code:
  # ffmpeg -i img_%03d.png ../video.mp4
  # number of frames to plot. This will be the amount of images saved.
  nFrames<-length(set_imgFiles)
  
  # Create directory if does not exist
  if(!dir.exists(out_path_images)) {dir.create(out_path_images)}
  
  # Stop function if dir is not empty
  if(!identical(dir(),character(0))){ stop("directory is not empty") }
  
  for(frameN in 1:nFrames){
    png(filename = paste0(out_path_images,"/img_", formatC(frameN,width=3,flag="0") ,".png"),res = dpi)
    plot(partIdenBee,frame=frameN,incThres = set_incThres)
    dev.off()
  }
}

# trackDem Commits

##' Background subtraction
##'
##' \code{subtractBackground} subtracts each image from a
##'  previously created still background.
##' The objects created through the function contain all changing
##' pixels (i.e. movement).
##' @param bg Array containing still background, as returned from
##' \code{\link{createBackground}}.
##' @param colorimages Array containing all frames, obtained by 
##' \code{\link{loadImages}}. Default is \code{NULL}, in this case the original
##' images are used from the global environment.
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' plot(allImages)
##'	}
##' @return Returns array of class 'TrDm' and 'sbg' with same size as images, 
##' subtracted from background.
##' @export

subtractBackground_in_iterator <- function (bg,colorimages=NULL) {
  
  if(is.null(colorimages)) { colorimages <- get(attributes(bg)$originalImages) }
  
  if(!is.TrDm(bg)) {
    ch1 <- all(dim(bg)[1:3] == dim(colorimages)[1:3])
    ch2 <- length(dim(bg)) %in% 3:4
    ch3 <- TRUE
    if (length(dim(bg)) == 4) ch3 <- dim(bg)[4] == dim(colorimages)[4]
    if (!all(ch1,ch2,ch3)) {
      stop(paste("Input does not appear to be of the class \"TrDm\"",
                 "and has wrong dimensions."))
    } else {
      message(paste("Input does not appear to be of the class \"TrDm\" but", 
                    "has the correct dimensions and is therefore used."))
    }
  }
  
  if (length(dim(bg)) == 3) {
    sbg <- array(NA,dim=dim(colorimages))
    for (i in 1:(dim(colorimages)[3])) {
      sbg[,,i,] <- sb(colorimages[,,i,],
                      bg[,,i],
                      dim(colorimages[,,i,]),
                      array(0,dim=dim(colorimages)))
    }
  } else if (length(dim(bg)) == 4) {
    sbg <- array(NA,dim=dim(colorimages))
    for (i in 1:(dim(colorimages)[3])) {
      sbg[,,i,] <- sb2(colorimages[,,i,],
                       bg[,,i,],
                       dim(colorimages[,,i,]),
                       array(0,dim=dim(colorimages)))
    }
  } else {stop("Wrong dimensions for background image.")} 
  
  attr(sbg,"background") <- deparse(substitute(bg))
  attr(sbg,"originalImages") <- attributes(bg)$originalImages
  attr(sbg,"originalDirec") <- attributes(bg)$originalDirec
  attr(sbg,"settings") <- attributes(bg)$settings
  
  class(sbg) <- c('TrDm','sbg','array')
  return(sbg)
}

##' Identify moving particles
##'
##' \code{identifyParticles} identifies moving particles using the 
##' subtracted images obtained from \code{\link{subtractBackground}}.
##' @param sbg Array containing images containing all moving particles,
##' as obtained from \code{\link{subtractBackground}}.
##' @param threshold Thresholds for including particles. A numeric vector
##' containing three values; one for each color. Otherwise, supply one value 
##' which is to be used for all three colors. For a chosen quantile
##' for each frame, use \code{qthreshold}. Default is \code{threshold=-0.1}, 
##'  which works for dark particles on a light background. Alternatively,
##' set \code{autoThres} below for an automatic threshold.  
##' @param pixelRange Default is \code{NULL}. Numeric vector with minimum and 
##' maximum particle size, used as a
##' first filter to identify particles. Use if particle of interest are of a 
##' known size range (in pixels).
##' @param qthreshold Default is \code{NULL}. Supply a value, to do thresholding 
##' based on quantile. Quantile is calculated for each
##' frame separately.
##' @param select Select dark particles (\code{'dark'}), light particles 
##' (\code{'light'}), or both (\code{'both'}), compared to background. 
##' Default is \code{'dark'}.
##' @param colorimages Array containing original color images. By default, the 
##' original color images are obtained from global environment.
##' @param autoThres Logical. \code{TRUE} to get an automated threshold for each 
##' color layer. Default is \code{FALSE}.
##' @param perFrame Logical. If \code{autoThres=TRUE}, set at \code{TRUE}
##'  to calculate a threshold for 
##' each frame separately. Default is \code{FALSE}. Note that is can be 
##' computationally intensive to calculate a threshold for each frame.
##' @param frames When \code{autoThres=TRUE} and \code{allFrames=FALSE}, supply a
##' numeric vector specifying over which frames the automated threshold 
##' should be calculated on (e.g. \code{c(1,3,5,7,9,11)} for all odd frames 
##' from 1 to 11). 
##' @author Marjolein Bruijning, Caspar A. Hallmann & Marco D. Visser
##' @examples
##' \dontrun{
##' dir.create("images")
##' ## Create image sequence
##' traj <- simulTrajec(path="images",
##'                     nframes=30,nIndividuals=20,domain="square",
##'                     h=0.01,rho=0.9,
##'                     sizes=runif(20,0.004,0.006))
##' ## Load images
##' dir <- "images"
##' allFullImages <- loadImages (dirPictures=dir,nImages=1:30)
##' stillBack <- createBackground(allFullImages,method="mean")
##' allImages <- subtractBackground(stillBack)
##' partIden <- identifyParticles(allImages,threshold=-0.1,
##'                                    pixelRange=c(3,400))
##' plot(partIden)
##' summary(partIden)
##'	}
##' @return Returns a dataframe of class 'TrDm' and 'particles', containing
##' particle statistics with identified particles for each frame
##' @export

identifyParticles_in_iterator <- function (sbg,threshold=-0.1,pixelRange=NULL,
                                           qthreshold=NULL,select='dark',
                                           colorimages=NULL,autoThres=FALSE,
                                           perFrame=FALSE,frames=NULL) {
  
  if(!is.TrDm(sbg)){
    stop("Input does not appear to be of the class \"TrDm\"")
  }
  if(is.null(colorimages)) { colorimages <- get(attributes(sbg)$originalImages) }
  namesbg <- deparse(substitute(sbg)) ## save for return
  tmp <- attributes(sbg)
  sbg <- aperm(sbg, c(1,2,4,3))
  attributes(sbg)$background <- tmp$background
  attributes(sbg)$originalImages <- tmp$originalImages
  attributes(sbg)$originalDirec <- tmp$originalDirec
  attributes(sbg)$settings <- tmp$settings
  
  cat("\t Particle Identification:  ")
  n <- 1:dim(sbg)[3]
  cat("\r \t Particle Identification: Thresholding (1 of 5)                 ",
      "           ")
  
  # if only one value supplied, use same value for each color layer
  if (length(threshold) == 1) {threshold <- rep(threshold,3)}
  
  # automated threshold
  if (autoThres) {
    cat("\r \t Particle Identification: Automated thresholding (1 of 5)     ",
        "       ")
    if (is.null(frames)) { frames <- n }
    threshold <- calcAutoThres(sbg[,,frames,],perFrame=perFrame)
  }
  
  if (!is.null(qthreshold)) {
    A <- array(NA,dim=dim(sbg))
    for (i in 1:3) { # over color layers
      A[,,,i] <- structure(vapply(seq_len(dim(sbg)[3]), function(x)
        sbg[,,x,i] < stats::quantile(sbg[,,x,i],qthreshold),
        numeric(prod(dim(sbg[,,1,1])))),dim=dim(sbg)[1:3])
    }
  } else {
    if (select == 'dark') {
      A <- structure(vapply(seq_along(threshold), 
                            function(x) sbg[,,,x] < threshold[x],
                            numeric(prod(dim(sbg[,,,1])))),
                     dim=dim(sbg))
    }
    else if (select == 'light') {
      A <- structure(vapply(seq_along(threshold), 
                            function(x) sbg[,,,x] > threshold[x],
                            numeric(prod(dim(sbg[,,,1])))),
                     dim=dim(sbg))
    }
    else if (select == 'both') {
      A <- structure(vapply(seq_along(threshold), 
                            function(x) sbg[,,,x] > threshold[x] |
                              sbg[,,,x] < -threshold[x],
                            numeric(prod(dim(sbg[,,,1])))),
                     dim=dim(sbg))
      
    }
    else {stop("Invalid selection, choose 'dark', 'light' or 'both' ")}
  }
  
  # 1 if 1 in at least one color layer
  sumRGB <- apply(A,c(2,3),rowSums)
  sumRGB <- sumRGB > 0
  
  cat("\r \t Particle Identification: Labeling (2 out of 5)                ",
      "     ")
  A <- array(NA,dim=dim(sumRGB))
  for (i in n) {
    A[,,i] <- SDMTools::ConnCompLabel(sumRGB[,,i])
  }
  
  cat("\r \t Particle Identification: Size filtering (3 out of 5)           ",
      "    ")
  if (!is.null(pixelRange)) {
    for (i in n) {
      allLabels <- tabulate(as.vector(A[,,i]),nbins=max(A[,,i]))
      allLabels <- allLabels[allLabels > 0]
      names(allLabels) <- sort(unique(as.numeric(A[,,i])))[-1]
      inc <- allLabels >= pixelRange[1] & allLabels <= pixelRange[2]
      A[,,i] [!A[,,i] %in% names(inc[inc==TRUE])] <- 0
    }
  }
  
  cat("\r \t Particle Identification: Particle statistics (4 out of 5)      ",
      "    ")
  
  dA <- dim(A[,,1]) ## get dim
  ## get total number of particles
  totalPart <- apply(A,3,function(x) length(unique(c(x)))-1) 
  psDim <- SDMTools::PatchStat(A[,,1])[-1,]
  
  particleStats <- matrix(NA,nrow=sum(totalPart),ncol=18)
  colnames(particleStats) <- c(colnames(psDim),
                               paste0('mu',c('R','G','B')),'x','y','frame')
  
  for (i in n) {
    if (i == 1) loc <- 1:totalPart[i]
    if (i > 1) {
      cum <- sum(totalPart[1:(i-1)])+1
      loc <- cum:(cum+totalPart[i]-1)
    }
    particleStats[loc,1:12] <- as.matrix(SDMTools::PatchStat(A[,,i])[-1,])
    
    particleStats[loc,13:15] <- as.matrix(
      extractMean(particleStats[loc,'patchID'],
                  colorimages=colorimages[,,,i],
                  images=A[,,i]))
    
    coords <- getCoords(m=A[,,i],d=dA)
    ind <- A[,,i] > 0
    particleStats[loc,'y'] <- tapply(coords[,1],A[,,i][ind],mean)
    particleStats[loc,'x'] <- tapply(coords[,2],A[,,i][ind],mean)
    particleStats[loc,'frame'] <- i
  }    
  particleStats <- as.data.frame(particleStats)
  
  cat("\r \t Particle Identification: Finalizing (5 out of 5)               ",
      "        \n ")
  
  attr(particleStats,"images") <- A
  attr(particleStats, "class") <- c("TrDm","particles","data.frame")
  attr(particleStats,"threshold") <- threshold
  attr(particleStats,"background") <- attributes(sbg)$background
  attr(particleStats,"originalImages") <- attributes(sbg)$originalImages
  attr(particleStats,"originalDirec") <- attributes(sbg)$originalDirec
  attr(particleStats,"subtractedImages") <- namesbg
  
  attr(particleStats,"settings") <- c(attributes(sbg)$settings,
                                      list(threshold=threshold,
                                           pixelRange=pixelRange,
                                           qthreshold=qthreshold,
                                           select=select,
                                           autoThres=autoThres,
                                           perFrame=perFrame,
                                           frames=frames))
  attr(particleStats,"nn") <- FALSE
  
  return(particleStats)
}

# RUN ANALYSIS ----
dirModifiedSource <- "src/trackdem_R_Commits/"
for(file in dir(dirModifiedSource)){
  source(paste0(dirModifiedSource,file))
}

library(trackdem) # Tengo que correr esos archivos mios primero y luego cargar el paquete
# trackSingleParticle_Master(dir_cutCoordsTable = "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny/thumbnails/crop_data.csv",
#                            stTime_cut = "00:00:04",
#                            endTime_cut = "00:00:20",
#                            framesPerIteration_ToIterations = 100,
#                            area_thr_toAnimation = 1000
# )
# dir_cutCoordsTable<-"/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/VideosToTryShiny/thumbnails/crop_data.csv"
# stTime_cut <- "00:00:04"
# endTime_cut <- "00:13:59"
# framesPerIteration_ToIterations=100
# area_thr_toAnimation=1000
