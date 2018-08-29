#    IPSUR: Introduction to Probability and Statistics Using R
#    Copyright (C) 2018 G. Jay Kerns
#
#    This file is part of IPSUR.
#
#    IPSUR is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    IPSUR is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with IPSUR.  If not, see <http://www.gnu.org/licenses/>.

# Preliminary code to load before start
# clear everything to start
rm(list = ls(all = TRUE))

# initial customizations
seed <- 42
set.seed(seed)
options(width = 60)
cexlab <- 1.5

# global knitr configuration
library(knitr)
opts_chunk$set(comment=NA)
opts_knit$set(width = 60)
