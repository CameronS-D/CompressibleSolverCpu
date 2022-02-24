from cgi import test
import subprocess
import os
from timeit import timeit
import openpyxl

def setup_worksheet(filename):
    if os.path.exists(filename):
        workbook = openpyxl.load_workbook(filename)
    else:
        workbook = openpyxl.Workbook()

    sheet = workbook.active
    return workbook, sheet

def save_to_worksheet(worksheet, col, result, row=None):
    if not row:
        # First first empty row in column
        row = 1
        while worksheet.cell(row=row, column=col).value:
            row += 1

    worksheet.cell(row=row, column=col).value = result

def change_nx_value(old_val: int, new_val: int):
    print(f"\n\nChanging nx from {old_val} to {new_val}\n")

    with open(f90_file, "r") as f:
        firstlines = ""
        for line_idx, line in enumerate(f):
            firstlines += line
            if line_idx == 9:
                break

        nx_line = f.readline()
        rest_of_file = f.read()

    nx_line = nx_line.replace(str(old_val), str(new_val), 1)

    full_file = firstlines + nx_line + rest_of_file

    with open(f90_file, "w") as f:
        f.write(full_file)

def is_output_correct(correct_code_cmd, test_code_cmd):
    print(f"Testing code output. Calling {correct_code_cmd} and {test_code_cmd}")
    proc = subprocess.run(correct_code_cmd, stdout=subprocess.PIPE)
    correct_output = proc.stdout

    proc = subprocess.run(test_code_cmd, stdout=subprocess.PIPE)
    test_output = proc.stdout

    print("Comparing output...")
    return correct_output == test_output


if os.path.isdir("code_timing"):
    os.chdir("code_timing")

f90_file = os.path.join("..", "2D_compressible.f90")
if not os.path.exists(f90_file):
    print(f"Error: Fortran file not found: {f90_file}")
    quit(1)

original_f90_file = "original_2D_compressible.f90"
if not os.path.exists(original_f90_file):
    print("Warning: Original code not found")
    original_f90_file = None

filename = os.path.join("execution_timings.xlsx")

workbook, timings_sheet = setup_worksheet(filename)

compiler_cmds = [
    ["gfortran -O3", "gfortran -O3"],
    ["gfortran -O3 -fopenmp", "gfortran -O3 -fopenmp"],
    ["gfortran -O3 -fopenmp -ftree-parallelize-loops=1", "gfortran -O3 -fopenmp 1 threads"],
    ["gfortran -O3 -fopenmp -ftree-parallelize-loops=2", "gfortran -O3 -fopenmp 2 threads"],
    ["gfortran -O3 -fopenmp -ftree-parallelize-loops=3", "gfortran -O3 -fopenmp 3 threads"],
    ["gfortran -O3 -fopenmp -ftree-parallelize-loops=4", "gfortran -O3 -fopenmp 4 threads"]
    ]

mesh_nx_options = [129, 257, 513, 1025, 2049]
reps = 5

for nx_option_idx in range(len(mesh_nx_options)):
    # Change value of nx in fortran code and add new nx row to spreadsheet
    change_nx_value(mesh_nx_options[nx_option_idx - 1], mesh_nx_options[nx_option_idx])
    nx_col = timings_sheet.min_column
    
    if nx_option_idx == 0:
        if timings_sheet.min_column != timings_sheet.max_column:
            next_row = timings_sheet.max_row+1
            for col in range(nx_col, timings_sheet.max_column + 1):
                save_to_worksheet(timings_sheet, col, "-", row=next_row)
        
        save_to_worksheet(timings_sheet, nx_col, "nx")

    save_to_worksheet(timings_sheet, nx_col, mesh_nx_options[nx_option_idx])

    next_excel_col = nx_col + 1

    for cmd, option_name in compiler_cmds:
        if nx_option_idx == 0:
            save_to_worksheet(timings_sheet, next_excel_col, option_name)

        if cmd.endswith(".bat"):
            compile_cmd = cmd
        else:
            compile_cmd = cmd + " -o output.exe " + f90_file
            original_code_cmd = cmd + " -o correct_output.exe " + original_f90_file

        print("\nCompiling using: " + compile_cmd)
        subprocess.run(compile_cmd.split(), stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

        if os.path.exists("output.exe"):
            print("Successfully compiled. Running output.exe")
        else:
            print("Compile failed. Moving to next compiler command.")
            next_excel_col += 1
            continue

        if mesh_nx_options[nx_option_idx] == 129:
            print("Compiling original code for comparison.")
            subprocess.run(original_code_cmd.split(), stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
            is_correct = is_output_correct("./correct_output.exe", "./output.exe")
            os.remove("./correct_output.exe")

            if is_correct:
                print("Code output is correct. Measuring runtime.")
            else:
                print("Incorrect output file produced. Moving to next compiler command.")
                save_to_worksheet(timings_sheet, next_excel_col, "N/A")
                next_excel_col += 1
                continue

        elapsed_time = timeit(
            stmt = "subprocess.run('./output.exe', stdout=subprocess.DEVNULL)",
            setup = "import subprocess",
            number = reps) / reps

        print(f"Saving elapsed time {elapsed_time:3f} to worksheet")
        save_to_worksheet(timings_sheet, next_excel_col, elapsed_time)

        os.remove("output.exe")
        next_excel_col += 1


print(f"\nCompleted test run. Saving worksheet to {filename}.")
workbook.save(filename)
