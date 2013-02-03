\name{play}
\title{play}
\alias{play}
\description{Function to Simulate car movement in BML model}
\usage{play(m,t)}
\arguments{
	  \item{m}{the BML object to perform the simulation on (created my map function)}
	  \item{t}{the number of time periods to run simulation over.}
}
\seealso{map}
\examples{
m = map(0.3,.5,100,100)	# create BML map
t = play(m,500)		# run simulation 500 times.
\dontrun{plot(t)}	# display dynamic simulation
}
