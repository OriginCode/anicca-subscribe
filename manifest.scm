(use-modules (gnu packages racket)
	     (gnu packages version-control)
	     (gnu packages fontutils)
	     (gnu packages base))

(packages->manifest (list racket-minimal git gnu-make fontconfig))
