# /Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackBees_shiny/trackBees


# Notas para el markdown
# Incluir ImageJ part
# Incluir peso y tiempo de pasos caros computacionalmente
# remover objetos no necesarios
# Incluir regresion de cuanto tiempo tarda por cuantos segundos
# incluir regresion de cuanta memoria gasta por cuantos segundos
# incluir video en el rmarkdown
# incluir shiny?

# VIDEO MANIPULATION WITH ffmpeg
# bees

# cd /Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/Movies
# cd /home/alvaro/Documents/STRI_Bees/TrackDem/TrackDem_Rproj/data/video_Bees
# rm Movies/bee_10s.mp4
# rm ImageSequence/*

# Extract one thumbnail (most representative frame) of video, to cut it with imageJ
# cd /Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/Movies
# ffmpeg -i originals/2018-05-12_16.00.25.h264.re-encoded.1024px.avi -vf  "thumbnail" -frames:v 1 img_toCropWith_imageJ/$filename%03d.png

# Dimensions of rectangle to cut
# y1=342
# y2=448
# x1=315
# x2=994
# Cut 10s AND
# Crop them to not have the hour and date moving, to solve problem

# cd /Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/Movies
# ls
# tree
## ffmpeg -ss 00:00:22 -i 2018-05-12_16.00.25.h264.re-encoded.1024px.avi -t 40 -filter:v "crop=iw:ih-260:0:260" bee_40s_imagecut.mp4
# ffmpeg -ss 00:00:04 -to 00:13:59 -i originals/2018-05-12_16.00.25.h264.re-encoded.1024px.avi -filter:v "crop=$x2-$x1:$y2-$y1:$x1:$y1" bee_14min_imagecut.mp4

# VOY POR AQUI, LA IDEA ES AHORA ANALIZAR EL VIDEO QUE EXPORTE DE 14MIN CROPPED
# DESPUES PODEMOS ANALIZAR EL VIDEO CON LOS DOS TRACKING A VER QUE PASA
# CUANDO LLEGUE VERE LOS RESULTADOS DE CORRER AMBOS TRACKS

# Create Image Seq at specified fps by argument -r
# cd /Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees
# ls
# tree
# Obtain images at fps=5
# ffmpeg -i Movies/bee_40s_imagecut.mp4 -r 5 ImageSequence/bee_40s_%04d.png
# ffmpeg -i Movies/bee_14min_imagecut.mp4 -r 5 ImageSequence/bee_14min/bee_14min_%04d.png

# R part ----
# Set directory and import image sequence
# iterating START ----
#library(R.utils)
library(png)
setwd("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj")
source("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/trackDem_Alvaro_source.R")
#library(trackdem)
#detachPackage("trackdem")
dirModifiedSource<-"/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/trackdem_R_Commits/"
for(file in dir(dirModifiedSource)){
  source(paste0(dirModifiedSource,file))
}
library(trackdem) # Tengo que correr esos archivos mios primero y luego cargar el paquete
framesInputDir_ToIterations <- "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/ImageSequence/bee_14min"
outputDir_ToIterations <- "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/docs/iterations_bee_14min"
framesPerIteration_ToIterations <- 100
dir(outputDir_ToIterations)
dir(framesInputDir_ToIterations)


rawTracked_bee14min <- iterate_partIden(set_Thr = -0.01,
                 set_PixelRange = c(0,10000),
                 framesInputDir = framesInputDir_ToIterations,
                 framesPerIteration = framesPerIteration_ToIterations,
                 outputDir = outputDir_ToIterations,
                 subsetOfIterations = 0)
format(object.size(rawTracked_bee14min),units="auto")
Sys.time()

rawTracked_bee14min<-read.csv("docs/iterations_bee_14min/TrackedVideo.csv")

track_data_from_identifyParticles <- rawTracked_bee14min
i = 1
f = 1
area_thr = 1000
imgFrames_dir <- "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/ImageSequence/bee_14min"
framesPerIteration = 100
prefix_imgs_files <- "bee_14min_"
fps_of_videoToimgs = 5
secs_of_delay_atStartOfVideo = 3
output_dir_framesAnimation ="docs/Animations/animation_iterations_bee_14min/"


# This is taking too long. We can subset the frames or ask python to do this job of creating the images. It is just a matter fo translating the code
createAnimations_fromIterations(track_data_from_identifyParticles = rawTracked_bee14min,
                                area_thr = area_thr,
                                imgFrames_dir = imgFrames_dir,
                                output_dir_framesAnimation = output_dir_framesAnimation,
                                framesPerIteration = framesPerIteration,
                                prefix_imgs_files = prefix_imgs_files,
                                fps_of_videoToimgs = fps_of_videoToimgs,
                                secs_of_delay_atStartOfVideo = secs_of_delay_atStartOfVideo)

# iterating END ----

# 1st iteration ----

png(filename = paste0(outPlotsDir,"1st_iteration_stillBack1.png"),res = dpi)
plot(stillBack1)
dev.off()

png(filename = paste0(outPlotsDir,"1st_iteration_partIdenBee1.png"),res = dpi)
plot(partIdenBee1)
dev.off()

png(filename = paste0(outPlotsDir,"1st_iteration_allImages1.png"),res = dpi)
plot(allImages1)
dev.off()

# png(filename = "docs/figures/1st_iteration_recordsBee1.png",res = dpi)
# plot(recordsBee1)
# dev.off()
# png(filename = "docs/figures/1st_iteration_recordsBee1.png",res = dpi)
# plot(recordsBee1,type="trajectories",incThres=set_incThres_ForPlot) #incThres =20 means just trajectories of more than that length will be included
# dev.off()

# Save summaries
# summ_recordsBee1 <- summary(recordsBee1)

# Save object sizes
# format(object.size(recordsBee1),units="auto")

runTime_1stIteration <- Sys.time() - st_2ndIteration
cat(paste("First interation runtime:\n",runTime_1stIteration))

# 1st iteration: Garbage collector ----
rm(list = c("allFullImages1", "stillBack1", "allImages1",
            "partIdenBee1"))
gc()

# 2nd iteration----
st_2ndIteration <- Sys.time()

st <- Sys.time()
allFullImages2 <- loadImages(dirPictures=framesInputDir,nImages=nImgs2)
runTime_loadImages2 <- Sys.time()-st
st <- Sys.time()
stillBack2 <- createBackground(allFullImages2,method='mean')
runTime_createBackground2 <- Sys.time()-st
st <- Sys.time()
allImages2 <- subtractBackground(bg=stillBack2)
runTime_substractBackground2 <- Sys.time()-st
st <- Sys.time()
partIdenBee2 <- identifyParticles(sbg=allImages2,
                                  pixelRange=set_PixelRange,# just moving objects between that range of pixel length
                                  select='dark',
                                  autoThres=FALSE,threshold=set_Thr)
runTime_identifyParticles2 <- Sys.time()-st
# st <- Sys.time()
# recordsBee2 <- trackParticles(partIdenBee2,L=set_R,R=set_R) # Should check R and L. Not using weight argument. L=50 gave one dummy particle.
# runTime_trackParticles2 <- Sys.time() - st 


# Save results: 2nd iteration ----
# Save Image of background extracted
png(filename =paste0(outPlotsDir, "2nd_iteration_stillBack2.png"),res = dpi)
plot(stillBack2)
dev.off()

png(filename = paste0(outPlotsDir,"2nd_iteration_partIdenBee2.png"),res = dpi)
plot(partIdenBee2)
dev.off()

png(filename = paste0(outPlotsDir,"2nd_iteration_allImagesBee2.png"),res = dpi)
plot(allImages2)
dev.off()

# png(filename = "docs/figures/2nd_iteration_recordsBee2.png",res = dpi)
# plot(recordsBee2)
# dev.off()
# png(filename = "docs/figures/2nd_iteration_recordsBee2.png",res = dpi)
# plot(recordsBee2,type="trajectories",incThres=set_incThres_ForPlot) #incThres =20 means just trajectories of more than that length will be included
# dev.off()

# Save summaries
summ_partIdenBee2 <- summary(partIdenBee2)
# summ_recordsBee2 <- summary(recordsBee2)

# Save object sizes
# format(object.size(recordsBee2),units="auto")
size_partIdenBee2 <- format(object.size(partIdenBee2),units="auto")

rawTrack_2 <- data.frame(partIdenBee2$x,partIdenBee2$y,partIdenBee2$patchID,partIdenBee2$area,partIdenBee2$frame,n_iteration=2)

runTime_2ndIteration <- Sys.time() - st_2ndIteration
cat(paste("Second interation runtime:\n",runTime_2ndIteration))

# 2nd iteration: Garbage collector ----
rm(list = c("allFullImages2", "stillBack2", "allImages2",
            "partIdenBee2"))
gc()

# Merge tracks ----

#recordsBee_1and2<-mergeTracks(recordsBee1,recordsBee2)
colnames(rawTrack_1) <-colnames(rawTrack_2) <-c("x","y","patchID","area","frame","n_iteration")
rawTrack_1and2 <- rbind(rawTrack_1,rawTrack_2)

# Results: plots ----
ggplot(filter(rawTracked_bee14min,area>15),aes(x,-y,col=frame))+geom_point()#+geom_line()
ggplot(filter(rawTracked_bee14min,area>1000,n_iteration==19),aes(x,-y,col=frame))+geom_point()+geom_line()

background<-readPNG("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/ImageSequence/bee_14min/bee_14min_0100.png")
dim(background)
#plot(background)

ggplot(filter(rawTrack_1and2,area>30),aes(x,dim(background)[1]-y,col=area))+background_image(background)+geom_point()+geom_line()+xlim(0,dim(background)[2])+ylim(0,dim(background)[1])
ggplot(filter(rawTrack_1and2,area>500),aes(x,dim(background)[1]-y,col=area))+background_image(background)+geom_point()+geom_line()+xlim(0,dim(background)[2])+ylim(0,dim(background)[1])
ggplot(filter(rawTracked_bee14min,area>5000,n_iteration==29),aes(x,dim(background)[1]-y,col=area))+background_image(background)+geom_point()+geom_line()+xlim(0,dim(background)[2])+ylim(0,dim(background)[1])

# To animation function
ggplot(filter(rawTracked_bee14min,area>5000,n_iteration==29),
       aes(x,dim(background)[1]-y,col=area))+
  background_image(background)+geom_point()+
  geom_line()+xlim(0,dim(background)[2])+
  ylim(0,dim(background)[1])
dfPlot<-filter(rawTracked_bee14min,area>5000,n_iteration==29)
plot(dfPlot$x,dfPlot)

filter(rawTracked_bee14min,area>3000,n_iteration==1)

#Replace the directory and file information with your info
background <- readPNG("/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/data/video_Bees/ImageSequence/bee_14min/bee_14min_0100.png")
bckg_height <- dim(background)[1]
bckg_width <- dim(background)[2]

# iterate frame
# iterate accross n_iteration and frame 


# ----


#Set up the plot area
plot(1:2, type='n', main="Plotting Over an Image", xlab="x", ylab="y")

#Get the plot information so the image will fill the plot box, and draw it
lim <- par()
rasterImage(background, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()
lines(-c(1, 1.2, 1.4, 1.6, 1.8, 2.0), -c(1, 1.3, 1.7, 1.6, 1.7, 1.0), type="b", lwd=5, col="white")+xlim(c(0,bckg_width))+ylim(c(0,-bckg_height))


# POR AQUI VAMOS. 
# HAY QUE DEFINIR PARTSIZE AT 5000, ES LA MEJOR

# HACER ANIMACION CON ESTOS PLOTS Y LOS FRAMES
# PODRIA SER UN FRAME POR SEGUNDO, SOLO PARA LA PARTE DE MOVIMIENTO

# TAL VEZ AGREGAR fps como argumento
# Tambien arreglar los frames; crear nueva variable columna
# que sea el tiempo en milisegundos, o segundos
# Explorar la posibilidad de excluir los puntos fuera del nido;
# Tal vez se podria dibujar un poligono y pedirle que evite cualquier cosa fuera de el poligono que es el nido
# Tambien hay que utilizar trajr, ese paquete tiene un suavizador
# ese suavizador nos dara la mejor estimacion de la posicion
# y con trajr y un par de operadores logicos, podemos llenar la informacion
# y hacer esa parte de "si desaparece y aparece en el mismo lugar, estuvo ahi siempre"
# creo que primero es esa parte y luego mandarlo al suavizador
# pero todo esto debe ser realizado junto con los plots
# los plots si duran mucho se podrian hacer en python
# al final, un Rmarkdown con el tiempo de corrida, la animacion, el heatmap.
# y una muestra y explicacion de cada parte, promete
# Disfrutemos estos dias. VAmos por buen camino
seqNumImgs<-(1:length(dir(framesInputDir_ToIterations)))
permutInts <- split(seqNumImgs, ceiling(seq_along(seqNumImgs)/100))
permutInts[5]

# mIDBee<-manuallySelect(partIdenBee,frame=12)
#plot(recordsBee2,type="trajectories",incThres=set_incThres_ForPlot) #incThres =20 means just trajectories of more than that length will be included

# Create Logfile ----
cat(paste("Behavioral ecology of Megalopta: videotracking.\nAuthor: alvaro vega hidalgo alvarovegahd@gmail.com \n","\n",
          "summ_partIdenBee1", summ_partIdenBee1,"\n",
          "summ_partIdenBee2", summ_partIdenBee2,"\n",
          "size_partIdenBee1",size_partIdenBee1,"\n",
          "runTime_1stIteration",runTime_1stIteration,"\n",
          "runTime_2ndIteration",runTime_2ndIteration), file = "docs/test.log", append = TRUE)

# Extract coordinates----

# Heatmap ----
## heat map
st<-Sys.time()

width <- 35
recordsBee
incLabels <- apply(recordsBee$sizeRecord,1,function(x) sum(!is.na(x)))>5
a <- recordsBee$trackRecord

# combine all x coordinates and y
x <- as.vector(a[incLabels,,1])
x <- x[!is.na(x)]
y <- as.vector(a[incLabels,,2])
y <- y[!is.na(y)]

## Grid count
l1 <- seq(0,ncol(allFullImages)+width,width) # full range 
l2 <- seq(0,nrow(allFullImages)+width,width) 
z <- as.matrix(table(cut(x,l1,include.lowest = TRUE),
                     cut(y,l2,include.lowest = TRUE)))

rownames(z)<-c(l1+(width/2))[-length(l1)]
colnames(z)<-c(l2+(width/2))[-length(l2)]


krigdat<- as.data.frame(z,stringsAsFactors=FALSE)
colnames(krigdat)<-c("x","y","count")
out <- kriging(as.numeric(krigdat$x),as.numeric(krigdat$y),
               response=krigdat$count,lags=2)
# predict heatmap
mat <- matrix(out$map$pred,ncol=length(unique(out$map$x)),
              nrow=length(unique(out$map$y)),byrow=FALSE)
rownames(mat) <- unique(out$map$y)
colnames(mat) <- unique(out$map$x)
plot(allFullImages,frame=(dim(allFullImages)[4])/2)
mat<-mat/sum(mat)
mat[mat<0]<-0
image(x=as.numeric(colnames(mat)) / ncol(allFullImages),
      y=sort(1-as.numeric(rownames(mat)) / nrow(allFullImages)),
      z=sqrt(t(mat[nrow(mat):1,])),
      col=topo.colors(100,alpha=.2),add=TRUE)
contour(x=as.numeric(colnames(mat)) / ncol(allFullImages),
        y=sort(1-as.numeric(rownames(mat)) / nrow(allFullImages)),
        z=(t(mat[nrow(mat):1,])),add=TRUE,col="white",
        drawlabels=FALSE,nlevels=5)

runTime_heatmap<-Sys.time()-st
runTime_heatmap

# Coordinates ----
track_coords <- data.frame(x,-y,time=1:length(x))

# trajR ----
plot(TrajFromCoords(track_coords))

# Create Imgs for animation----
source("trackDem_Alvaro_source.R")
alvaro_createImgsForAnimation(identifiedParticles = partIdenBee,
                              out_path_images = "docs/Animations/bee_40s_imagecut",set_imgFiles = lengthImgFiles)
# R CODE: create video ----
# patternNameOfFile <- "docs/Animations/animation_iterations_bee_14min/bee_14min_%04d.png" # should be a local directory or subdirectory, not global; starting from getwd()
# p <- patternNameOfFile
# out <- "bee_14min.mp4"
# # 10 fps gif
# ffmpeg(pattern = p, output = out, rate = 100,overwrite = T,size = "600x100")

# UNIX Code: create video ----
# cd /Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/docs/Animations/bee_40s_imagecut
 ffmpeg -i img_%03d.png ../bee_40s_imagecut.mp4
# rm *.png

# Errors ----

# Bugs in trackdem

# At creating image seqs

# createImageSeq(imagepath="data/v0.4/imageSequence/", 
#                moviepath="data/video_Bees/Movies/originals",
#                exiftoolpath = "/Users/krichilskyea/Documents/Alvaro/trackDem_Bees/trackDem_Bees_Rproj/exiftool",
#                 fps=5, nsec=2, ext="avi")
#,libavpath="/usr/bin/avconv",
#                exiftoolpath="/Users/krichilskyea/Documents/Alvaro/install_Mac/Image-ExifTool-11.03/exiftool")#, 
#               pythonpath="/usr/bin/python2.7")
# createImageSeq(imagepath="/home/alvaro/Documents/STRI_Bees/TrackDem/TrackDem_Rproj/data/video_Ony/managedByTrackdem/ImageSequences/", moviepath="/home/alvaro/Documents/STRI_Bees/TrackDem/TrackDem_Rproj/data/video_Ony/managedByTrackdem/Movies/",fps=15, nsec=2, ext="MOV")

# At creating animations
# plot(recordsBee,type="animation",incThres=20,path = "docs/Animations/bee_10s/")
# unlink("docs/Animations/bee_10s/images",recursive = T)
# dev.off()

# Notes
# plot(partIdenBee,frame=9,incThres = 20)
# png("img3.png",res=800)
# plot(partIdenBee,frame=10)
# dev.off()
# plot(partIdenBee,frame=11)
# plot(partIdenBee,frame=12)
# plot(partIdenBee,frame=13)
# This is to count particles I think?
# nEstBee <- apply(recordsBee1$sizeRecord,2,function(x) sum(!is.na(x)))

# Onys ----

# Onys
# Slow Down video
# ffmpeg -an -i Ony12_C1.MOV -r 16 -filter:v "setpts=0.125*PTS" -strict -2 Ony12_C1_4x.MOV
# cd /home/alvaro/Documents/STRI_Bees/TrackDem/TrackDem_Rproj/data/video_Ony/managedByTrackdem/Movies

# ls
# ffmpeg -i Ony12_C1_8x.MOV -ss 00:00:30 -t 00:00:40 -async 1 -strict -2 Ony12_C1_8x_10s.MOV

# Commits trackdem ----
# Gausian filter to images when want to track one single particle
# Might not be the best idea, because it won't be able to detect subtle movement

# recordsBee_1and2<-mergeTracks(recordsBee1,recordsBee2)
# Error in seq_len(nrow(coords1)) : 
#   argument must be coercible to non-negative integer
# In addition: Warning message:
#   In seq_len(nrow(coords1)) : first element used of 'length.out' argument

#  exiftool path not found but it is right there