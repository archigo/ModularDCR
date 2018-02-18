module Activity 

// Types and auxiliaries for human-friendly activity description language (HF-ADL; or maybe COmmon Activity-Oriented Language?)

type segment = 
| Input of string
| Output of string
| 
