test_mzRBackend <- function() {
    res <- mzR:::.mzRBackend("test.mzml")
    checkEquals(res, "pwiz")
    res <- mzR:::.mzRBackend("test.mzml.gz")
    checkEquals(res, "pwiz")
    res <- mzR:::.mzRBackend("test.mzML")
    checkEquals(res, "pwiz")
    res <- mzR:::.mzRBackend("test.mzML.bz2")
    checkEquals(res, "pwiz")
    res <- mzR:::.mzRBackend("test.mzXML")
    checkEquals(res, "pwiz")

    res <- mzR:::.mzRBackend("test.mzdata")
    checkEquals(res, "Ramp")
    res <- mzR:::.mzRBackend("test.mzdata.gz")
    checkEquals(res, "Ramp")
    
    res <- mzR:::.mzRBackend("test.cdf")
    checkEquals(res, "netCDF")
    res <- mzR:::.mzRBackend("test.cdf.gz")
    checkEquals(res, "netCDF")

    checkException(mzR:::.mzRBackend("unsupported.txt"))
    checkException(mzR:::.mzRBackend())
    checkException(mzR:::.mzRBackend(""))
    checkException(mzR:::.mzRBackend(c("a.mzML", "b.mzML")))
}

test_mzRBackendFromContent <- function() {
    library(msdata)
    f <- system.file("cdf/ko15.CDF",  package = "msdata")
    checkEquals(mzR:::.mzRBackendFromContent(f), "netCDF")
    mzf <- system.file("proteomics/TMT_Erwinia_1uLSike_Top10HCD_isol2_45stepped_60min_01.mzML.gz", package = "msdata")
    checkEquals(mzR:::.mzRBackendFromContent(mzf), "pwiz")
    f <- system.file("microtofq/MM14.mzML", package = "msdata")
    checkEquals(mzR:::.mzRBackendFromContent(f), "pwiz")
    f <- system.file("microtofq/MM14.mzData", package = "msdata")
    checkEquals(mzR:::.mzRBackendFromContent(f), "Ramp")
    f <- system.file("microtofq/MM14.mz5", package = "msdata")
    checkEquals(mzR:::.mzRBackendFromContent(f), "pwiz")
}
