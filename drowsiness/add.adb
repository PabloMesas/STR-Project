
--------------------------------------------
-----             STR 2018             -----
-----       Pablo Mesas Lafarga        -----
-----    Alejandro Mendez Fernandez    -----
--------------------------------------------

with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;
with System; use System;
with Workload;

-- Packages needed to generate pulse interrupts       
with Ada.Interrupts.Names;
with Pulse_Interrupt; use Pulse_Interrupt;

package body add is

    Time_per_Kwhetstones : Ada.Real_Time.Time_Span :=
                            Ada.Real_Time.Nanoseconds (2273176); -- anterior (479936);
    ---------------------------------------------------------------------
    precision_error : constant float := 0.1; -- measure in +-%
    step : float := 10.0;
    time_executed : constant Time_span := Ada.Real_Time.Milliseconds(100);
    t1 : Time;
    time_lapsed : Time_span;
    t2 : Time;

    ---------------------------------------------------------------------

    ---------------------------------------------------
    ------------- procedure exported 
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ---------------------------------------------------------------------
    --     PROCEDIMIENTO QUE HACE CALCULOS                             --
    ---------------------------------------------------------------------

    procedure Execution_Time (Time : Ada.Real_Time.Time_Span) is

    begin
        Workload.Small_Whetstone (Time / Time_per_Kwhetstones);
    end Execution_Time;
    ---------------------------------------------------------------------
    ---------------------------------------------------------------------
    --     PROCEDIMIENTO QUE SACA EL VALOR DE UN ENTERO POR LA UART    --
    ---------------------------------------------------------------------

    procedure Print_an_Integer (x : in integer) is
     begin
      --Put ("(");
      Kernel.Serial_Output.Put (Integer'Image(x));
      --Put (")");
    end Print_an_Integer;

    procedure Print_a_Float (x : in float) is
       type Float_Printable is digits 2; 
       nx: Float_Printable;
     begin
      --Put ("(");
      nx := Float_Printable (x);
      Kernel.Serial_Output.Put (Float_Printable'Image(nx));
      --Put (")");
    end Print_a_Float;

    ---------------------------------------------------------------------
    function is_error_good(error : float) return Boolean is
        begin
            if error <= precision_error then
                return True;
            else 
                return False;
            end if;
    end is_error_good;

    function adjust_error(t : Ada.Real_Time.Time_Span) return Boolean is
        error : float;
        begin
            if t < time_executed then
                error := float(to_duration(t)) / float(to_duration(time_executed));
                Time_per_Kwhetstones := Time_per_Kwhetstones + Ada.Real_Time.Nanoseconds (Integer(step * error));
            elsif t > time_executed then
                error := float(to_duration(time_executed)) / float(to_duration(t));          
                Time_per_Kwhetstones := Time_per_Kwhetstones - Ada.Real_Time.Nanoseconds (Integer(step * error));
            end if;

            error := (1.0 - error) * 100.0;
            Print_an_Integer(Integer(error * 100.0));
            return is_error_good(error);
    end adjust_error;


begin
    Print_an_Integer(Integer(float(to_duration(Time_per_Kwhetstones)) * 1000000000.0));
    New_Line;
    loop
        t1 := clock;
        Execution_time(time_executed);
        t2 := clock;
        time_lapsed := t2 -t1;
        exit when adjust_error(time_lapsed);
    end loop;

    Print_an_Integer(Integer(float(to_duration(time_lapsed)) * 1000.0));
    New_Line;
    Print_an_Integer(Integer(float(to_duration(Time_per_Kwhetstones)) * 1000000000.0));

end add;
