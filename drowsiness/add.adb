
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;

with Tools; use Tools;
with Devices; use Devices;

-- Packages needed to generate pulse interrupts       
-- with Ada.Interrupts.Names;
-- with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    -----------------------------------------------------------------------
    ------------- declaration of tasks 
    -----------------------------------------------------------------------

    procedure Electrodes;

    procedure Eyes_Detection; 

    ----------------------------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------

    ---------------------------------------------------------------------
    Procedure Electrodes  is 
        R: EEG_Samples_Type;
    begin
         Starting_Notice ("Electrodes"); 
         Reading_Sensors (R);
         Display_Electrodes_Sample (R);
         Finishing_Notice ("Electrodes");
    end Electrodes;

    ---------------------------------------------------------------------
    Procedure Eyes_Detection is
        Current_R: Eyes_Samples_Type;
    begin
         Starting_Notice ("Eyes_Detection");
         Reading_EyesImage (Current_R);
         Display_Eyes_Sample (Current_R);
         Beep (3);
         Finishing_Notice ("Eyes_Detection");
    end Eyes_Detection;


begin
   null;
   Electrodes;
   Eyes_Detection;
end add;



