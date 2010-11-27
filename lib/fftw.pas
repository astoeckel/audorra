unit fftw;
{
   FFTW - Fastest Fourier Transform in the West library

   This interface unit is (C) 2005 by Daniel Mantione
     member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This file carries, as a independend work calling a well
   documented binary interface, the Free Pascal LGPL license
   with static linking exception.

   Note that the FFTW library itself carries the GPL license
   and can therefore not be used in non-GPL software.
}

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$CALLING cdecl} {Saves some typing.}

{$MACRO on}
{$INLINE on}

{$IFDEF Unix}
  const
    fftwlib = 'fftw3';
{$ELSE}
  const
    fftwlib = 'libfftw3';
{$ENDIF}

type    complex_double=record
          re,im:Double;
        end;
        Pcomplex_double=^complex_double;

        fftw_plan=type pointer;

        fftw_sign=(fftw_forward=-1,fftw_backward=1);

        fftw_flag=(fftw_measure,            {generated optimized algorithm}
                   fftw_destroy_input,      {default}
                   fftw_unaligned,          {data is unaligned}
                   fftw_conserve_memory,    {needs no explanation}
                   fftw_exhaustive,         {search optimal algorithm}
                   fftw_preserve_input,     {don't overwrite input}
                   fftw_patient,            {generate highly optimized alg.}
                   fftw_estimate);          {don't optimize, just use an alg.}
        fftw_flagset=set of fftw_flag;
                   

{Complex to complex transformations.}
function fftw_plan_dft_1d(n:cardinal;i,o:Pcomplex_double;
                          sign:fftw_sign;flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_1d';
function fftw_plan_dft_2d(nx,ny:cardinal;i,o:Pcomplex_double;
                          sign:fftw_sign;flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_2d';
function fftw_plan_dft_3d(nx,ny,nz:cardinal;i,o:Pcomplex_double;
                          sign:fftw_sign;flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_3d';

function fftw_plan_dft(rank:cardinal;n:Pcardinal;i,o:Pcomplex_double;
                       sign:fftw_sign;flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft';

{Real to complex transformations.}
function fftw_plan_dft_1d(n:cardinal;i:Pdouble;o:Pcomplex_double;
                          flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_r2c_1d';
function fftw_plan_dft_2d(nx,ny:cardinal;i:Pdouble;o:Pcomplex_double;
                          flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_r2c_2d';
function fftw_plan_dft_3d(nx,ny,nz:cardinal;i:Pdouble;o:Pcomplex_double;
                          flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_r2c_3d';
function fftw_plan_dft(rank:cardinal;n:Pcardinal;i:Pdouble;o:Pcomplex_double;
                       flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_r2c';

{Complex to real transformations.}
function fftw_plan_dft_1d(n:cardinal;i:Pcomplex_double;o:Pdouble;
                          flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_c2r_1d';
function fftw_plan_dft_2d(nx,ny:cardinal;i:Pcomplex_double;o:Pdouble;
                          flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_c2r_2d';
function fftw_plan_dft_3d(nx,ny,nz:cardinal;i:Pcomplex_double;o:Pdouble;
                          flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_c2r_3d';
function fftw_plan_dft(rank:cardinal;n:Pcardinal;i:Pcomplex_double;o:Pdouble;
                       flags:fftw_flagset):fftw_plan;
         external fftwlib name 'fftw_plan_dft_c2r';


procedure fftw_destroy_plan(plan:fftw_plan);
          external fftwlib name 'fftw_destroy_plan';
procedure fftw_execute(plan:fftw_plan);
          external fftwlib name 'fftw_execute';

{$calling register} {Back to normal!}
procedure fftw_getmem(var p:pointer;size:sizeint);
procedure fftw_freemem(p:pointer);inline;

{*****************************************************************************}
                                  implementation
{*****************************************************************************}

{$ifndef Windows}
{$LINKLIB fftw3}
{$endif}

{Required libraries by libfftw3}
{ $LINKLIB gcc}
{ $LINKLIB c}
{ $LINKLIB m}

{Better don't use fftw_malloc and fftw_free, but provide Pascal replacements.}

{$IF defined(cpui386) or defined(cpupowerpc)}
  {$DEFINE align:=16}
{$ENDIF}

procedure fftw_getmem(var p:pointer;size:sizeint);

{$IFDEF align}
var
  originalptr:pointer;
begin
  { We allocate additional "align-1" bytes to be able to align.
    And we allocate additional "SizeOf(Pointer)" to always have space to store
    the value of the original pointer. }
  getmem(originalptr,size + align-1 + SizeOf(Pointer));
  ptruint(p):=(ptruint(originalptr) + SizeOf(Pointer));
  ptruint(p):=(ptruint(p)+align-1) and not (align-1);
  PPointer(ptruint(p) - SizeOf(Pointer))^:=originalptr;
{$ELSE}
begin
  getmem(p,size);
{$ENDIF}
end;

procedure fftw_freemem(p:pointer);inline;

begin
{$IFDEF align}
  freemem(PPointer(ptruint(p) - SizeOf(Pointer))^);
{$ELSE}
  freemem(p);
{$ENDIF}
end;

end.
