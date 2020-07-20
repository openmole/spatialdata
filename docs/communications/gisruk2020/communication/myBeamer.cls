\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{myBeamer}
\LoadClass{beamer}



%%% Language
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[english]{babel}



%%% Colors
\usepackage{xcolor}
\definecolor{primaryDarkOM}{HTML}{3F3D56}
\definecolor{primaryGreenOM}{HTML}{A6BF26}

\definecolor{accentDarkOM}{HTML}{2F2E41}
\definecolor{accentRedOM}{HTML}{F86D70}



%%% Page layout
\usepackage{multicol}
\usepackage{graphicx}
\usepackage{tikz}
\usepackage{etoolbox}
\usepackage[absolute]{textpos}
\usepackage{setspace}
\usepackage{xmpmulti}



%%% Font style
\renewcommand{\familydefault}{lmss}

\usepackage{quoting}
\usepackage{xparse}

\newcommand{\refs}[3]{
	{\tiny \textit{#1 et al. #2#3}}\hspace{-0.25cm}
}
\newcommand{\head}[1]{
	{\color{primaryGreenOM}\textbf{#1}}
}



%%% Math
\usepackage{mathtools}
\usepackage{physics}
\everymath{\displaystyle}
\usepackage{gensymb}
\usepackage{xfrac}



%%% Tables
\usepackage{float}
\usepackage{multirow}
\usepackage{makecell}
\setcellgapes[t]{3pt}
\renewcommand{\theadfont}{\bfseries}
\renewcommand{\theadgape}{}
\usepackage{diagbox}
\usepackage{colortbl}



%%% Theme
\usetheme{Darmstadt}
\setbeamertemplate{headline}{}
%\setbeamertemplate{footline}[frame number]
\setbeamertemplate{footline}{
		\leavevmode%
		\hbox{%
			\begin{beamercolorbox}[wd=.3\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}
				\insertshortauthor
		  	\end{beamercolorbox}%
		  	\begin{beamercolorbox}[wd=.4\paperwidth,ht=2.25ex,dp=1ex,center]{frametitle}
		    	\insertshorttitle
			\end{beamercolorbox}%
			\begin{beamercolorbox}[wd=.3\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}
				\insertdate
		  \end{beamercolorbox}%
		}%
		\vskip0pt%
	}


\setbeamertemplate{navigation symbols}{}


%% Logo
\newcommand{\addlogo}{
	\hfill	
	\includegraphics[height=.4cm]{figures/logos/iconOM.png}
	\includegraphics[scale=1]{figures/logos/openmole_dark.png}
}

\newcommand{\sframe}[2]{\frame{\frametitle{#1\addlogo} #2}}


%% Color theme
% Palette
\setbeamercolor{palette primary}{fg=primaryGreenOM, bg=primaryDarkOM}
\setbeamercolor{palette secondary}{fg=primaryDarkOM, bg=primaryGreenOM}
%\setbeamercolor{palette tertiary}{fg=primaryGreenOM, bg=primaryDarkOM}
%\setbeamercolor{palette quaternary}{fg=primaryDarkOM, bg=primaryGreenOM}

\setbeamercolor{normal text}{fg=primaryDarkOM}
%\setbeamercolor{title}{fg=primaryDarkOM, bg=primaryGreenOM}
\setbeamercolor{item}{fg=primaryGreenOM}
\setbeamertemplate{itemize items}[triangle]
\setbeamertemplate{enumerate items}[default]
\setbeamercolor{section in toc}{fg=primaryGreenOM}

% Block
\setbeamercolor{block title}{fg=primaryDarkOM, bg=primaryGreenOM}
\setbeamercolor{block body}{bg=white}
\setbeamercolor{block title alerted}{fg=accentDarkOM, bg=accentRedOM}
\setbeamercolor{block body alerted}{bg=white}
\AtBeginEnvironment{alertblock}{\setbeamercolor{item}{fg=accentRedOM}}


%% Footer
%\setbeamertemplate{footline}{
%	\leavevmode%
%	\hbox{%
%		\begin{beamercolorbox}[wd=.3\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}
%			\insertshortauthor
%	  	\end{beamercolorbox}%
%	  	\begin{beamercolorbox}[wd=.4\paperwidth,ht=2.25ex,dp=1ex,center]{frametitle}
%	    	\insertshorttitle
%		\end{beamercolorbox}%
%		\begin{beamercolorbox}[wd=.3\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}
%			\insertdate
%			\hspace{.5cm}
%			\insertframenumber / \inserttotalframenumber
%	  \end{beamercolorbox}%
%  }%
%  \vskip0pt%
%}


%% Section page
\setbeamertemplate{section page}{
	\begin{center}
		{\LARGE \color{primaryGreenOM}\bfseries\insertsection}
	\end{center}
}

%\setbeamertemplate{frametitle}{
%	\insertframetitle
%	\hfill
%	\insertlogo
%}


%% Backup
\newcommand{\backupbegin}{
	\newcounter{framenumberappendix}
	\setcounter{framenumberappendix}{\value{framenumber}}
	\setbeamertemplate{footline}{
		\leavevmode%
		\hbox{%
			\begin{beamercolorbox}[wd=.3\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}
				\insertshortauthor
		  	\end{beamercolorbox}%
		  	\begin{beamercolorbox}[wd=.4\paperwidth,ht=2.25ex,dp=1ex,center]{frametitle}
		    	\insertshorttitle
			\end{beamercolorbox}%
			\begin{beamercolorbox}[wd=.3\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}
				\insertdate
		  \end{beamercolorbox}%
		}%
		\vskip0pt%
	}
}

\newcommand{\backupend}{
	\addtocounter{framenumberappendix}{-\value{framenumber}}
	\addtocounter{framenumber}{\value{framenumberappendix}}
%	\setbeamertemplate{footline}{
%		\vspace{-1cm}\small{\insertframenumber/\inserttotalframenumber}
%	}
}

