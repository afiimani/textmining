#My File for pdf convertion and text mining
# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.04.zip
# and extract to your program files folder
#Source for Pdf conversion: https://gist.github.com/benmarwick/11333467

# Tell R what folder contains your 1000s of PDFs
dest <- "/Users/afiimani/Desktop/HUBS/BD Spokes 2 page pre-proposals/"

# make a vector of PDF file names
myfiles <- list.files(path = dest, pattern = "pdf",  full.names = TRUE)

# Convert Pdf to text
lapply(myfiles, function(i) system(paste('"/Applications/xpdfbin-mac-3.04/bin32/pdftotext"', paste0('"', i, '"')), wait = FALSE) )

# where are the txt files you just made?
dest # in this folder

# And now you're ready to do some text mining on the text files

