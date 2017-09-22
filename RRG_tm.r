#My File for pdf convertion and text mining
# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.04.zip
# and extract to your program files folder

# set path to pdftotxt.exe and convert pdf to text
exe <- "C:\\Program Files\\xpdfbin-win-3.04\\bin32\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

