# Lee el .cshrc del sistema

source /usr/glob/user/.cshrc
#######################################################################
# 				.cshrc
#######################################################################

# Variables de entorno:

#setenv XENVIRONMENT $HOME/bin/menu.dir/menu.def

# Variables de directorios

set texi    = ~/tex.dir
set idlsoft = ~/idlsoft.dir
set stokes  = ~/stokes.dir
set bin     = ~/bin
set contri  = ~/stokes.dir/contri.dir
set inver   = ~/stokes.dir/inversion.dir
set mallas  = ~/stokes.dir/mallas.dir
set model   = ~/stokes.dir/model.dir
set program = ~/stokes.dir/program.dir
set ejec    = ~/stokes.dir/program.dir/ejec.dir
set lejec   = ~/discolocal/program.dir/ejec.dir
set lib     = ~/stokes.dir/program.dir/lib.dir
set fuentes = ~/stokes.dir/program.dir/fuentes.dir
set con     = ~/stokes.dir/program.dir/fuentes.dir/con.dir
set iper    = ~/stokes.dir/program.dir/fuentes.dir/iper.dir
set respu   = ~/stokes.dir/respu.dir
set hoy     = ~/hoy.dir
set basi    = ~/tex.dir/basi.dir
set iconos  = ~/icon.dir
set basura  = /scratch/brc
set responerror = /scratch/brc/program.dir/fuentes.dir/responerror

#set path    = ($path $ejec $bin $con $fuentes)
#set path    = (/usr/local/bin /usr/local/lib /usr/local/etc /usr/local/sbin $path) 
#setenv LD_LIBRARY_PATH /usr/local/lib $LD_LIBRARY_PATH


#set noclobber               # Proteccion de sobreescritura con ">"
set filec                   # Variable de continuacion de paths (con Esc)

limit coredumpsize 0

# Alias

alias cd            'cd \!*;set prompt = "%M:%C3> "'
alias   pwd     'echo $cwd'

if ($?USER == 0 || $?prompt == 0) exit

alias	bye	logout
alias	.	'ls -l'
alias	help	man
alias	ty 	more	
alias	his	history
alias   ls	'ls -F'
#alias   netscape  'netscape -install -iconic -mail'
alias   basi    'cd $texi/basi.dir'
alias   purge    'rm *%'
alias purgetex	    'rm *dvi; rm *log; rm *aux'

alias scrtizon 'cd /scratch/brc'
alias simula 'cd /scratch/brc/sir_gaussianas/simulacion'
alias incremento 'cd /scratch/brc/sir_gaussianas/simulacion/incremento8'
alias suave 'cd /home/brc/SIR/PC_piloto/PC_suave'
alias suave2 'cd /home/brc/SIR/PC_piloto/PC_suave2'
alias plasma 'cd /home/brc/tex.dir/basi.dir/plasma'
alias ldo100 'cd /net/tizon/scratch/brc/Sunrise/survey/ldo100'

alias gran16  'cd /net/tizon/scratch/brc/granulacion16'
alias scrcamello  'cd /net/camello/scratch1/brc'
alias SEA  'cd /net/camello/scratch1/brc/SEA'

     setenv  MANPATH /usr/lang/man:/usr/man:/usr/local/man:/usr/openwin/man
     setenv  HELPPATH /usr/lang/SC1.0/SourceBrowser.info
     setenv  IDL_STARTUP ~/IDL
     
     setenv FC ifort 
     setenv LINK ifort
     setenv F77 ifort

     # SETTINGS FOR CFITSIO    
     setenv LD_LIBRARY_PATH /scratch/brc/SIR_Parallel/cfitsio/cfitsio-install/lib:$LD_LIBRARY_PATH

# SETTINGS FOR MPICH         
     setenv PATH /scratch/brc/SIR_Parallel/mpich-3.1.4/mpich-install/bin:$PATH
     setenv LD_LIBRARY_PATH /scratch/brc/SIR_Parallel/mpich-3.1.4/mpich-install/lib:$LD_LIBRARY_PATH
     set history=40
     set ignoreeof

#######################################################################
