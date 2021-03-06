% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DecisionRule.R
\name{DecisionRule}
\alias{DecisionRule}
\title{R6 class representing a decision rule.}
\description{
A decision rule is an implication extracted from an information table.
}
\details{
A decision rule is made up of a complex condition and class assignments.
For STAT1 type of rules the conditions are lower bounds, and assignment goes to an upward class union.
For STAT2 type of rules the conditions are upper bounds, and assignment goes to a downward class union.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{condition}}{the left-hand side of the implication - a complex condition}

\item{\code{t}}{the right-hand side of the implication - t-parameter of class union(s)}

\item{\code{type}}{the right-hand side of the implication - (class union or union of classes)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{DecisionRule$new()}}
\item \href{#method-isMinimal}{\code{DecisionRule$isMinimal()}}
\item \href{#method-isWeaker}{\code{DecisionRule$isWeaker()}}
\item \href{#method-ruleMetrics}{\code{DecisionRule$ruleMetrics()}}
\item \href{#method-print}{\code{DecisionRule$print()}}
\item \href{#method-getRHS_String}{\code{DecisionRule$getRHS_String()}}
\item \href{#method-toList}{\code{DecisionRule$toList()}}
\item \href{#method-toString}{\code{DecisionRule$toString()}}
\item \href{#method-equals}{\code{DecisionRule$equals()}}
\item \href{#method-clone}{\code{DecisionRule$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\subsection{Method \code{new()}}{
Create a new DecisionRule object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$new(condition, t, type)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{condition}}{the complex condition}

\item{\code{t}}{the t-parameter of the class union(s)}

\item{\code{type}}{the type of the rule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-isMinimal"></a>}}
\subsection{Method \code{isMinimal()}}{
Method to decide if this decision rule is minimal among the provided set of rules.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$isMinimal(it, rules)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{it}}{the information table to use}

\item{\code{rules}}{the set of rules to check the minimality in}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a boolean value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-isWeaker"></a>}}
\subsection{Method \code{isWeaker()}}{
Method to decide if this decision rule is a weaker implication compared to the provided rule.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$isWeaker(it, rule)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{it}}{the information table to use}

\item{\code{rule}}{the rule to compare to}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a boolean value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ruleMetrics"></a>}}
\subsection{Method \code{ruleMetrics()}}{
Method to calculate rule support, certainty, coverage, and strength
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$ruleMetrics(it)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{it}}{the information table to use}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a named list of integers
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\subsection{Method \code{print()}}{
print method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getRHS_String"></a>}}
\subsection{Method \code{getRHS_String()}}{
toString method for the RHS of the rule.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$getRHS_String()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toList"></a>}}
\subsection{Method \code{toList()}}{
Method to convert this decision rule to a list, which can be merged into a dataframe for a list of decision rules.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$toList(it)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{it}}{the information table to use}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a named list with the LHS, RHS, and the metrics of this rule
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-toString"></a>}}
\subsection{Method \code{toString()}}{
toString method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$toString()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-equals"></a>}}
\subsection{Method \code{equals()}}{
Method to compare two DecisionRules
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$equals(other)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{other}}{the other decision rule to compare to}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a single boolean value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DecisionRule$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
