The 2 last numbers in the Atomic parameters file (for instance LINES)
are needed to evaluate the collisional damping taking into account collisions 
with H atoms following Barklem's formula.

The first number is alpha and the second sigma

The table table1 has been download from
Barklem, Piskunov, O'Mara**/* *Astron. Astrophys. Suppl. Ser./ 142, 467-473
(see ReadMe file)

In this table the units of sigma are the square of Bohr's Radius, 
so to write in SIR units this value has to be multiplied by
2.8002e-17

For instance for the line  5250.2 we have in the table
   sigma=207.
   alpha=0.253
   and, so for SIR we would write
5250.2089    1.2    0.121     -4.96    5D 0.0- 7D 1.0  0.253    5.79600e-15
 
There are some lines that do not appear in this table. In this case
there is a program fortran written by Barklem
withcomp.f (using the subroutine  retcross.f)
To compile 
ifort -i8 widthcomp.f retcross.f -o widthcomp.out

as input introdue a file like
lines.input

the program should write a file like 
lines.long


