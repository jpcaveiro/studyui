

library(stringr)


CLOZE_PATTERN = '\\{(\\d*):([^:]+):([^}]+)\\}'



#cloze_match <- str_match_all(cloze_text, CLOZE_PATTERN)[[1]]
#cloze_match <- str_match_all("cloze_text", CLOZE_PATTERN)[[1]]

get_cloze_type <- function(tag) {
  
  #is done before
  #tag <- toupper(tag)

  if (tag %in% c("SHORTANSWER", "SA",  "MW")) {
    
    # short answers (SHORTANSWER or SA or MW), case is unimportant,
    tag <- "SHORTANSWER"
    
  } else if (tag %in% c("SHORTANSWER_C", "SAC", "MWC")) {
    
    # short answers (SHORTANSWER_C or SAC or MWC), case must match,
    tag <- "SHORTANSWER_C"
    
  } else if (tag %in% c("NUMERICAL", "NM")) {
    
    # numerical answers (NUMERICAL or NM),
    tag <- "NUMERICAL"
    
  } else if (tag %in% c("MULTICHOICE", "MC")) {
    
    # multiple choice (MULTICHOICE or MC), represented as a dropdown menu in-line in the text,
    tag <- "MULTICHOICE"
    
  } else if (tag %in% c("MULTICHOICE_V", "MCV")) {
    
    # multiple choice (MULTICHOICE_V or MCV), represented as a vertical column of radio buttons, or
    tag <- "MULTICHOICE_V"
    
  } else if (tag %in% c("MULTICHOICE_H", "MCH")) {
    
    # multiple choice (MULTICHOICE_H or MCH), represented as a horizontal row of radio-buttons,
    tag <- "MULTICHOICE_H"
    
  } else if (tag %in% c("MULTIRESPONSE", "MR")) {
    
    # multiple choice (MULTIRESPONSE or MR), represented as a vertical row of checkboxes
    tag <- "MULTIRESPONSE"
    
  } else if (tag %in% c("MULTIRESPONSE_H", "MRH")) {
    
    # multiple choice (MULTIRESPONSE_H or MRH), represented as a horizontal row of checkboxes
    tag <- "MULTIRESPONSE_H"
    
  } else if (tag %in% c("MULTICHOICE_S", "MCS")) {
    
    # multiple choice (MULTICHOICE_S or MCS), represented as a dropdown menu in-line in the text,
    tag <- "MULTICHOICE_S"
    
  } else if (tag %in% c("MULTICHOICE_VS", "MCVS")) {
    
    # multiple choice (MULTICHOICE_VS or MCVS), represented as a vertical column of radio buttons, or
    tag <- "MULTICHOICE_VS"
    
  } else if (tag %in% c("MULTICHOICE_HS", "MCHS")) {
    
    # multiple choice (MULTICHOICE_HS or MCHS), represented as a horizontal row of radio-buttons.
    tag <- "MULTICHOICE_HS"
    
  } else if (tag %in% c("MULTIRESPONSE_S", "MRS")) {
    
    # multiple choice (MULTIRESPONSE_S or MRS), represented as a vertical row of checkboxes
    tag <- "MULTIRESPONSE_S" 

  } else if (tag %in% c("MULTIRESPONSE_HS", "MRHS")) {
    
    # multiple choice (MULTIRESPONSE_HS or MRHS), represented as a horizontal row of checkboxes
    tag <- "MULTIRESPONSE_HS" 
    
  } else {
    
    tag <- "no tag"
    
  }
  
  return(tag)
}



parse_one_option <- function(opt_text) {

  #answer <- ""
  #feedback <- ""
  
  
  #get percent valid
  if (substr(opt_text,1,1) == '=') {
    
    percent <- "100"
    opt_text <- substr(opt_text, 2, nchar(opt_text))
  
  } else if (substr(opt_text,1,1) == '%') {
    
    percent_match <- str_match(opt_text, "^%([-+]?\\d+)%(.+)")
    #percent_match <- percent_match[[1]]
    
    
    num_of_matches <- length(percent_match) #/ 4
    
    if (num_of_matches > 0) {
      percent  <- percent_match[[1,2]]
      opt_text <- percent_match[[1,3]]
    } else {
      percent <- "0"
    }
  } else {
    
    percent <- "0"
    
  }
  

  #Find feedback
  text_hash_feedback <- strsplit(opt_text, '#')[[1]]
  if (length(text_hash_feedback) == 1) {
    
    #No hastag 
    answer <- text_hash_feedback
    feedback <- ""
    
  } else if (length(text_hash_feedback) == 2) {
    
    #answer and feedback
    answer   <- text_hash_feedback[1]
    feedback <- text_hash_feedback[2]
    
  } else {   
    
    #probabl feedback contains hastag
    #TODO: possible error
    answer <- text_hash_feedback[1]
    feedback <- paste(text_hash_feedback[2:length(text_hash_feedback)], collapse = "#")
  }
  
  
  o = list("percent"=percent, "answer"=answer, "feedback"=feedback)

  return(o)
}




cloze2regexcloze <- function(cparts) {
  #cparts is short for cloze_question_parts
  #cloze_question_parts <- list("points"=question_points, "type"=question_type, "options"=list_options)
  
  options_text <- c()
  for (o in cparts$options) {
    #o = list("percent"=percent, "answer"=answer, "feedback"=feedback)
    options_text <- c(options_text, paste0("%",o$percent,"%",o$answer,"#",o$feedback,collapse=""))
  } 
  options_text <- paste0(options_text,collapse="~")
  
  return(paste0("{",cparts$points,":",cparts$type,":",options_text))
}



cloze2html <- function(cloze_question_parts) {
  # Ver C:\Users\pedrocruz\Documents\GitHub\estatisticaaplicada\bookdown\interaction.R
  
  return(cloze_question_parts)
  
}
  


#' Parse one cloze instruction 
#' 
#' Instructions are like {:SHORTANSWER:=Paris#Correct!~*#Wrong answer}
#' and breaks them into its parts.
#'
#' @param cloze_instruction - instructions has here ...moodle cloze page....
#' @param output - see line: `if (output=="cloze") {``
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{cloze_instruction <- "{:SHORTANSWER:=Paris#Correct!~*#Wrong answer}"}
#' \dontrun{print(parse_one_cloze_instruction(cloze_instruction, output="html"))}
#' \dontrun{cloze_instruction <- "{1:MCV:%50%Jupiter#Partial Correct!~=Mars#Correct~Saturn#Wrong}"}
#' \dontrun{print(parse_one_cloze_instruction(cloze_instruction, output="html"))}
parse_one_cloze_instruction <- function(cloze_instruction, output="cloze") {
  #    The function only checks the syntax of CLOZE expressions.

  #cloze_match <- str_match_all(cloze_instruction, CLOZE_PATTERN)[[1]]
  cloze_match <- str_match(cloze_instruction, CLOZE_PATTERN)
  
  if (length(cloze_match) / 4 != 1) {
    return(paste("parse_cloze_instruction: function must be applied to a single cloze instruction. Check", cloze_instruction,"\n", colappse=""))
  }
  
  m <- 1 # only match
  
  question_fullstr <- cloze_match[m,1]
  question_points <- cloze_match[m,2] #poins
  if (question_points == "") {
    question_points <- "1"
  }
  question_type   <- toupper(cloze_match[m,3]) #instruction
  question_content<- cloze_match[m,4] #configuration
  

  tag <- get_cloze_type(question_type)
  if (tag == "no tag") {
    cat("CLOZE ERROR: ", question_fullstr, "\n")
    cat("  Instruction '", question_type, "' does not exist (please, check spelling).\n", sep="")
    next
  } else {
    question_type <- tag
  }

  #Debug
  #cat("CLOZE: ", cloze_match[m,1], "\n")
  #cat("  Points: ", question_points, "\n")
  #cat("  Type: ", question_type, "\n")
  #cat("  Content: ", question_content, "\n")

      
  # #Normalize use of: ~, =, %100%, etc.
  # #=resposta correta~incorreta#feedback
  # #Text should not contain those symbols, yet.
  options <- strsplit(question_content, "~")[[1]]
  
  if (length(options) == 0) {
    cat("CLOZE Error: question '", question_fullstr, "without options.\n", sep="")
    next
  }  

  list_options <- c()
  for (opt_text in options) { 


    #Debug
    #print(opt_text)
    
    #TODO: protect from mistakes like "<no text>#Some feedback" 
    o <- parse_one_option(opt_text)
    
    list_options <- append(list_options, list(o))
  }
  
  #Verifica se há 100 em alguma a opção :
  # tot = [o.percent>=100 for o in all_options]
  tot  <- sapply(list_options, function(o) o["percent"] >= 100)

  if (!any(tot)) {
    cat("CLOZE ERROR: in '", question_fullstr, "' there is no correct option.\n", sep="")
    next
  }
  
  cloze_question_parts <- list("points"=question_points, "type"=question_type, "options"=list_options)
  #Debug
  #print(q)


  
  if (output=="originalcloze") {
    return(cloze_match[m,1])
  } else if (output=="regexcloze") {
    return(cloze2regexcloze(cloze_question_parts))
  } else if (output=="html") {
    return(cloze2html(cloze_question_parts))
  } else {
    return(cloze_question_parts)
  }
}
 




#' Transform text written in cloze format intro something else.
#' 
#'
#' @param cloze_text 
#' @param output 
#'
#' @returns
#' @export
#'
#' @examples
cloze_transform <- function(cloze_text, output="originalcloze") {
  matches_info <- gregexpr(CLOZE_PATTERN, cloze_text, perl = TRUE)[[1]]
  
  #print(matches_info[1])
  #print(attr(matches_info,"match.length"))
  
  
  n_clozematches <- length(matches_info)
  cloze_start    <- c(matches_info)
  cloze_len      <- c(attr(matches_info,"match.length"))
  
  
  cloze_text_parts <- c()
  
  text_pos <- 1
  
  # Situations:
  #
  # 1. text{instruction}text{instruction}text
  # 2. {instruction}text{instruction}text
  # 3. text{instruction}text{instruction}
  # 4. {instruction}{instruction}
  
  
  for(m in 1:n_clozematches) {
    
    if (text_pos < cloze_start[m]) {
      cloze_text_parts <- c(cloze_text_parts, substr(cloze_text, text_pos, cloze_start[m]-1))
    } else {
      #não é adicionado texto porque text_pos = cloze_start
    }
    
    # call parse_one_cloze_instruction()
    if (output == "I") {
      instruction <- paste(rep("I",cloze_len[m]),collapse="")
    } else {
      original_instruction <- substr(cloze_text, cloze_start[m], cloze_start[m]+cloze_len[m]-1)
      instruction <- parse_one_cloze_instruction(original_instruction, output)
    }
    
    cloze_text_parts <- c(cloze_text_parts, instruction)
    
    #Prepare next
    text_pos <- cloze_start[m] + cloze_len[m]
  }
  cloze_text_length <- nchar(cloze_text)
  if (text_pos<=cloze_text_length) {
    cloze_text_parts <- c(cloze_text_parts, substr(cloze_text, text_pos, cloze_text_length))
  }
  
  
  
  return(cloze_text_parts)
  
}


             #0        1         2         3         4         5         6         7         8         9        10        11        12        13        14        15        16        17
             #12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
cloze_text = "The capital of France is {:SHORTANSWER:=Paris#Correct!~*#Wrong answer}. The largest planet in the solar system is {1:MCV:%50%Jupiter#Partial Correct!~=Mars#Correct~Saturn#Wrong}."
print(cloze_transform(cloze_text, output="regexcloze"))

