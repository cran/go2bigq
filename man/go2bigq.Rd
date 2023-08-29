\name{go2bigq}
\alias{go2bigq}

\title{
Function to convert various numerics to \code{bigq}with full precision.  
}
\description{
Convert a numeric, a character string representing a numeric, or various \code{mpfr} classes to a \code{bigq} fraction. The output has the same precision as the input.   
}
\usage{
go2bigq(x,  ...)
}

\arguments{
  \item{ x}{
  A value of class \code{numeric}, \code{mpfr}, \code{mpfr1}, or \code{character}. A list containing any one of these classes is allowed.   See Details for various limitations  
}

  \item{...}{	
Parameters which could be used to modify the default behavior of \code{formatMpfr}.  See Details for more information. 
}
 
}
\details{
The input \code{x} , if a list, must contain only one of the allowed classes. 
Character strings which represent numbers must be of the form [+,-]X[.]Y[e,E[+,-]Z], where X, Y, and Z are strictly limited to the numerals [0:9]. None of  the other character options listed are required (except of course "[+,-]Z" is disallowed unless "[e,E]" exists.)
The function \code{\link[Rmpfr]{formatMpfr}} accepts several arguments to adjust the output character string (number of digits precision, e.g.). \code{go2bigq} passes the \code{...} objects to \code{formatMpfr}.  

 
}
\value{
A \code{bigq} object containing a vector of \code{bigq} fractions .  
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
\code{\link[Rmpfr]{.bigq2mpfr}}, \code{\link[Rmpfr]{.bigz2mpfr}}, \code{\link[gmp]{as.bigq}}
}

\examples{
library(Rmpfr)
(go2bigq(0.17 + 1:5))
(go2bigq(as.character(0.17 + 1:5))) 
(go2bigq(mpfr(0.17 + 1:5,100))) 
as.numeric(go2bigq('017.3200e-10')[1] - go2bigq(0017.3200e-10)[1])


}
