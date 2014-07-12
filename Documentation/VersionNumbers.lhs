\documentclass[11pt]{article}

\usepackage{amsfonts}

\newcommand{\release}{\ensuremath{\mathcal{R}}}
\newcommand{\nextrelease}{\ensuremath{\mathcal{R}^{+1}}}
\newcommand{\version}{\ensuremath{\mathcal{V}}}
\newcommand{\nextversion}{\ensuremath{\mathcal{V}^{+1}}}
\newcommand{\package}{\ensuremath{\mathcal{P}}}
\newcommand{\revision}{\ensuremath{\mathbb{R}}}

\begin{document}

\section{Terms}

\begin{tabular}{|l|l|}
\hline
Symbol & Description \\
\hline
\hline
\release & current live release \\
\hline
\nextrelease & upcoming release \\
\hline 
\package & a package \\
\hline
\version & a version \\
\hline
\nextversion & the next version \\
\hline
\revision & a revision \\
\hline
\end{tabular}

\section{Version Number Policy}

Where \release\ is the release field and \revision\ is the revision in that dist. 

\begin{table}[h]
\begin{tabular}{|l|l||l|}
\hline
Condition & Rule & Example \\
\hline 
\hline 
\package\ from Debian   &  \textit{upstream}-\textit{debian}r{\release}freespire\revision & 1.0-1r2freespire1 \\
\hline
\package\ is not in Debian &  \textit{upstream}-0r{\release}freespire\revision & 1.0-0r2freespire1 \\
\hline
We are upstream & \textit{upstream}-0r{\release}freespire\revision (???) & 1.0-0r2freespire1 (???) \\
\hline
Downstream from us & \textit{upstream}-\textit{debian}r\textit{\release}{freespire}\revision you\revision &  1.0-1r2freespire1jackdonaldson1 \\
\hline
\end{tabular}
\end{table}

\section{Use Cases}
\subsection{Add a new package to \nextrelease}

\begin{itemize}
\item \release\ is released, and \nextrelease\ is under development.
\item $(\package, \version)$ is released by the upstream maintainer.
\item (\package,\version) is put in \nextrelease, and not in \release.
\end{itemize}

\subsection{Simple Package Update}

\begin{itemize}
\item $(\package, \version)$ is in \release\ and \nextrelease.
\item $(\package, \nextversion)$ is released.
\item $(\package, \nextversion)$ is updated in \release\ and \nextrelease.
\end{itemize}


\subsection{\package\ does not build in \nextrelease}

$(\package,\version)$ is in \release.
$(\package,\version)$ in not in \nextrelease\ because it does not build for \nextrelease.

Two cases:
\begin{enumerate}
\item $(\package, \version)$ can be installed on \nextrelease\ because the binary dependencies are satisfied
\item $(\package, \version)$ can not be installed on \nextrelease\ because the binary dependencies are not satisfied
\end{enumerate}

\begin{itemize}
\item What happens when the user upgrades from $\release \rightarrow \nextrelease$
\end{itemize}

\subsection{$(\package, \nextversion)$ is updated in \release\ only}

\begin{itemize}
\item \release\ and \nextrelease\ contain $(\package, \version)$.
\item $(\package, \nextversion)$ is released.
\item $(\package, \nextversion)$ is updated in \release\ only.
\end{itemize}

Consider the case when the binary $(\release, \package, \nextversion)$ will not install on \nextrelease.

\subsection{Outside Developers}

Developers who will be creating dists on their own servers that
Freespire users will be clicking and running from.

Cartesian product of:

\begin{itemize}
\item We have already modified the package 
\item We have not yet modified the package
\end{itemize}

\begin{itemize}
\item We should be upstream of the developer in the version number
\item The developer should be upstream of us in the version number
\end{itemize}

\subsection{Rebuilds}

\begin{itemize}
\item $(\package, \version)$ is in \release\ and \nextrelease.
\item build dependency changes force a rebuild of \package\ only in \release.
\end{itemize}

\subsection{Same upstream source, different Linspire patches}

We build $(\package,\version)$ for \release\ and \nextrelease, but with a
slightly different set of patches.

\subsection{Pulling package from $\release \rightarrow \nextrelease$ with extra patches}

We take $(\release,\package,\version)$ and build it for \nextrelease with
some extra patches.

\subsection{What if the upstream revision ends with a non-numeric character?}

\section{Who Manages Changelog?}

We want to be able to do a lot of local rebuilds with out worry about
debian/changelog. But a build that is 'release' worthy, should have a
hand-made changelog entry. Since the changelog entry is hand-made, the
version number could also be add at that time using something lnike
dch, or the emacs changelog mode.

\section{Other}

Building multiple related targets that exist only on the the local
disk. aka, not committed yet.


\end{document}