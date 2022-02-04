
bscresults.addErrorBarsUp <- function (x, y, ebu, length = 0.08, ...)
    arrows(x, y + ebu, x, y, angle = 90, code = 3, length = length, ...)

