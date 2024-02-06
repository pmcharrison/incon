# MIT License
#
# Copyright (c) 2022 Santiago Barreda
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

reduce_fraction <- function(ratio) {
  if (length(ratio) != 2)
    stop("Ratio must contain two numbers.")
  if (ratio[1] %% 1 > 0 | ratio[2] %% 1 > 0)
    stop("Elements of ratio must be whole numbers.")
  numerator = ratio[1]
  denominator = ratio[2]
  remainder = -1
  while (remainder != 0) {
    remainder = numerator %% denominator
    numerator = denominator
    if (remainder != 0)
      denominator = remainder
  }
  ratio = ratio / denominator
  return(ratio)
}
