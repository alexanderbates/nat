context("neuronlistfh")

test_that("neuronlistfh behaves like a neuronlist",{
  kcs5=kcs20[1:5]
  tf=tempfile('kcs20fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=tf),'neuronlistfh')
  
  # check that ordering is maintained
  expect_equal(names(kcs20fh),names(kcs20))
  expect_equal(kcs20fh[1:5],kcs5)
  
  # check that l/sapply works and produces equivalent results
  expect_equal(lapply(kcs20fh,length),lapply(kcs20,length))
  expect_equal(sapply(kcs20fh,length),sapply(kcs20,length))
  
  # check subset
  expect_equal(subset(kcs20,type=='gamma'),subset(kcs20fh,type=='gamma'))
  
  # mirror points
  kcs20m=mirror(kcs20,mirrorAxisSize=500,transform='flip')
  kcs20fhm=mirror(kcs20fh,mirrorAxisSize=500,transform='flip')
  expect_equal(kcs20m,kcs20fhm)
  
  # arithmetic
  kcs20t=kcs20+1
  kcs20fht=kcs20fh+1
  expect_equal(kcs20t,kcs20fht)
  
  # data.frames
  expect_equal(kcs20fh[1:5,], kcs20[1:5,])
  
  # extended attributes - nb a real regtemplate is a more complex object
  attr(kcs5, 'regtemplate')="FCWB"
  attr(kcs20fh, 'regtemplate')="FCWB"
  expect_equal(kcs20fh[1:5], kcs5)
})

test_that("subset neuronlistfh without data.frame",{
  kcs5=kcs20[1:5]
  data.frame(kcs5)=NULL
  tf=tempfile('kcs5fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs5fh<-as.neuronlistfh(kcs5, dbdir=tf),'neuronlistfh')
  expect_is(kcs5again<-subset(kcs5fh, names(kcs5)), 'neuronlist')
  # it was the names that disappeared
  expect_equal(kcs5again, kcs5)
})

test_that("we can load a previously created on disk neuronlistfh representation",{
  # create on disk filehash with one file per neuron
  fhpath=tempfile(pattern='kcs20fh')
  dir.create(file.path(fhpath,'data'),recursive=T)
  kcs20fh=as.neuronlistfh(kcs20, dbdir=file.path(fhpath, 'data'), dbClass='RDS')
  plot3d(subset(kcs20fh,type=='gamma'), soma=T)
  on.exit(unlink(fhpath,recursive=TRUE))
  
  # now save and reload 
  tf=tempfile()
  tf2=tempfile()
  on.exit(unlink(c(tf,tf2)),add=TRUE)
  write.neuronlistfh(kcs20fh,file=tf)
  saveRDS(kcs20fh,file=tf2)
  # ensure that write.neuronlistfh and saveRDS produce identical file
  expect_equivalent(tools::md5sum(tf),tools::md5sum(tf2))
  kcs20fh2=read.neuronlistfh(tf)
  # the only difference between the two objects should be the file attribute
  # added by read.neuronlistfh
  attr(kcs20fh2,'file')=NULL
  expect_equal(kcs20fh,kcs20fh2)
  expect_equal(as.neuronlist(kcs20fh),as.neuronlist(kcs20fh2))
})

test_that("we can create a neuronlistfh with a hashmap",{
  fhpath=tempfile(pattern='kcs20fh')
  on.exit(unlink(fhpath,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=fhpath, hashmap=TRUE),
            'neuronlistfh')
  expect_is(attr(kcs20fh,'hashmap'),'environment')
  
  expect_equal(lapply(kcs20fh,length),lapply(kcs20,length))
  expect_equal(sapply(kcs20fh,length),sapply(kcs20,length))
})

test_that("we can create a neuronlistfh without rewriting objects",{
  # make a neuronlistfh
  fhpath=tempfile(pattern='kcs20fh')
  fhdatapath=file.path(fhpath,'data')
  dir.create(fhdatapath,recursive=TRUE)
  on.exit(unlink(fhpath,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=fhdatapath), 'neuronlistfh')
  
  # then make a new copy after removing one file
  kfm=attr(kcs20fh,'keyfilemap')
  unlink(file.path(fhdatapath,kfm[1]))
  # check we are missing one file
  expect_equal(length(dir(fhdatapath)),length(kfm)-1)
  
  # that we don't replace it when WriteObjects='no'
  expect_is(kcs20fh2<-as.neuronlistfh(kcs20, dbdir=fhdatapath, WriteObjects='no'),
            'neuronlistfh')
  expect_equal(length(dir(fhdatapath)),length(kfm)-1)
  
  expect_is(kcs20fh3<-as.neuronlistfh(kcs20, dbdir=fhdatapath, 
                                      WriteObjects='missing'), 'neuronlistfh')
  # and that we put it back when WriteObjects='missing'
  expect_equal(length(dir(fhdatapath)),length(kfm))
  
  expect_equal(sapply(kcs20fh3,length),sapply(kcs20,length))
})

test_that("read.neurons(nlfh) == as.neuronlist(nlfh)",{
  tf=tempfile('kcs20fh')
  on.exit(unlink(tf,recursive=TRUE))
  expect_is(kcs20fh<-as.neuronlistfh(kcs20, dbdir=tf), 'neuronlistfh')
  
  expect_equal(as.neuronlist(kcs20fh), read.neurons(kcs20fh))
})

context("neuronlistfh remote")

skip_cran_no_internet()

test_that("we can download a neuronlistfh object with MD5'd objects", {
  localdir <- tempfile()
  dir.create(localdir)
  on.exit(unlink(localdir, recursive=TRUE))
  kcs20.url="http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds"
  kcs20md5 <- read.neuronlistfh(kcs20.url, localdir=localdir, quiet=TRUE)
  # test trying to read in neuronlistfh object which is now available locally
  # before we have downloaded any data objects
  kcs20md5.2 <- read.neuronlistfh(kcs20.url, localdir=localdir, quiet=TRUE)
  expect_equal(dim(kcs20md5[[1]]$points), c(284, 3))
  
  # test updating the neuronlistfh object after messing up the current version
  writeLines('Rhubarb crumble!', attr(kcs20md5.2, 'file'))
  expect_error(read.neuronlistfh(kcs20.url, localdir=localdir))
  expect_message(kcs20md5.3<-read.neuronlistfh(kcs20.url, localdir=localdir, 
                                               quiet=TRUE, update=TRUE),
                 "Updating cached")
  expect_equal(kcs20md5.3, kcs20md5.2)
})

test_that("we can synchronise a neuronlistfh object with its remote", {
  localdir <- tempfile()
  dir.create(localdir)
  on.exit(unlink(localdir, recursive=TRUE))
  kcs20fh.remote <- read.neuronlistfh("http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/flycircuit/kcs20.rds",
                                      localdir=localdir)
  expect_equal(dim(kcs20fh.remote[[1]]$points), c(284, 3))
  # make a neuronlistfh object from the local data bundled with this package
  # pointing the database directory to the same location as kcs20fh.remote
  kcs20fh.local=as.neuronlistfh(kcs20, dbdir=attr(kcs20fh.remote, 'db')@dir)

  kfm=attr(kcs20fh.remote,'keyfilemap')
  dbdir=attr(kcs20fh.remote, 'db')@dir
  files_before=dir(dbdir)
  # now sync (nothing should happen, only object)
  remotesync(kcs20fh.remote)
  files_after=dir(dbdir)
  expect_equal(files_before,files_after)
  
  # now sync (nothing should happen since there should be no missing files)
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE)
  files_after=dir(dbdir)
  expect_equal(files_before,files_after)
  
  # delete a file and check it is downloaded
  unlink(file.path(dbdir,files_before[1]))
  remotesync(kcs20fh.remote, update.object=FALSE)
  expect_equal(files_before[-1],dir(dbdir))
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE)
  expect_equal(files_before,dir(dbdir))
  
  # delete a file and check that we can sync when specifying indices
  unlink(file.path(dbdir,files_before[1]))
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE, 
             indices='rhubarb')
  expect_equal(files_before[-1],dir(dbdir))
  remotesync(kcs20fh.remote, update.object=FALSE, download.missing=TRUE, 
             indices=names(kcs20fh.remote))
  expect_equal(files_before,dir(dbdir))
    
  # add an extra file and check it is deleted
  tf=tempfile(tmpdir=dbdir)
  writeLines('rhubarb',con=tf)
  remotesync(kcs20fh.remote, update.object=FALSE, delete.extra=TRUE)
  expect_equal(files_before,dir(dbdir))
  
  # check that we can synchronise when just giving path to object on disk
  expect_equal(remotesync(attr(kcs20fh.remote,'file'),
                          update.object=FALSE,download.missing=TRUE),
               kcs20fh.remote)
})
