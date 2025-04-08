library(pdftools)

convert_pdf_to_txt <- function(pdf_file_path, output_name) {
  pdf_text_content = pdf_text(pdf_file_path)
  full_text = paste(pdf_text_content, collapse = "\n\n")
  writeLines(full_text, con = output_name)

  cat("Saved to", output_name, "\n")
}

# Directory containing your PDFs
pdf_dir = "C:/Users/samng/Downloads/aging pdf"

# Get a list of PDF files
pdf_files = list.files(pdf_dir, pattern = "*.pdf", full.names = TRUE)

# Loop through each PDF file and convert
for (i in seq_along(pdf_files)) {
  # Generate output file name (e.g., 001.txt, 002.txt, ...)
  output_file_name = sprintf("%03d.txt", i)

  # Convert PDF to TXT
  convert_pdf_to_txt(pdf_files[i], output_file_name)
}