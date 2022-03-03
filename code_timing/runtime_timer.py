from cgi import test
from readline import write_history_file
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
        letter_col = openpyxl.utils.get_column_letter(col)
        col_cells = [cell for cell in worksheet[letter_col]]

        # Find last cell in column with a value
        row = worksheet.max_row
        for cell in col_cells[::-1]:
            if cell.value is not None:
                break
            row -= 1
        # We need the cell after which is empty
        row += 1

    worksheet.cell(row=row, column=col).value = result

def change_nx_value(filename, old_vals, new_val):
    # Read in full file
    with open(filename, "r") as f:
        firstlines = ""
        for line_idx, line in enumerate(f):
            firstlines += line
            if line_idx == 9:
                break

        nx_line = f.readline()
        rest_of_file = f.read()

    # Change nx value on single line
    for old_val in old_vals:
        nx_line = nx_line.replace(str(old_val), str(new_val), 1)

    full_file = firstlines + nx_line + rest_of_file

    # Write out file
    with open(filename, "w") as f:
        f.write(full_file)

def is_output_correct(correct_output, test_code_cmd):
    print(f"Testing code output. Calling test code: {test_code_cmd}")

    proc = subprocess.run(test_code_cmd, stdout=subprocess.PIPE, encoding="UTF-8")
    test_output = proc.stdout

    print("Comparing output...")
    cor_output_list = correct_output.split()
    test_output_list = test_output.split()
    if len(cor_output_list) != len(test_output_list):
        print("Error: Test code output is too short.")
        print(f"Correct length: {len(cor_output_list)}, Test length: {len(test_output_list)}")
        return False 

    for cor_line, test_line in zip(correct_output.split(), test_output.split()):
        if cor_line[:10] != test_line[:10]:
            print("Error: Test code value is incorrect.")
            print(cor_line[:10])
            print(test_line[:10], "\n")
            return False
    return True

def get_correct_output(filename):
    # Compile
    original_code_cmd = "gfortran -O3 -o correct_output.exe " + filename
    subprocess.run(original_code_cmd.split(), stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
    # Run output
    proc = subprocess.run("./correct_output.exe", stdout=subprocess.PIPE, stderr=subprocess.STDOUT, encoding="UTF-8")
    # Tidy up
    os.remove("./correct_output.exe")

    return proc.stdout

if os.path.isdir("code_timing"):
    os.chdir("code_timing")

f90_file = os.path.join("..", "2D_compressible.f90")
if not os.path.exists(f90_file):
    print(f"Error: Fortran file not found: {f90_file}")
    quit(1)

original_f90_file = "original_2D_compressible.f90"
if not original_f90_file or not os.path.exists(original_f90_file):
    print("Warning: Original code not found")
    original_f90_file = None

filename = os.path.join("execution_timings.xlsx")

workbook, timings_sheet = setup_worksheet(filename)

compiler_cmds = [
    # ["gfortran -O3", "gfortran -O3"],
    # ["ifort -O3", "ifort -O3"],
    # ["ifort -O3 -parallel", "ifort -O3 -parallel"],
    # ["ifort -O3 -fopenmp", "ifort -O3 -fopenmp"]
    # ["gfortran -O3 -fopenmp", "gfortran -O3 -fopenmp"],
    # ["gfortran -O3 -fopenmp -ftree-parallelize-loops=1", "gfortran -O3 -fopenmp 1 threads"]
]

mesh_nx_options = [129, 257, 513, 1025, 2049]
last_nx_val = mesh_nx_options[-1]
reps = 5

next_excel_col = timings_sheet.min_column
if timings_sheet.min_column != timings_sheet.max_column:
    next_row = timings_sheet.max_row+1
    for col in range(next_excel_col, next_excel_col + len(compiler_cmds) + 1):
        save_to_worksheet(timings_sheet, col, "-", row=next_row)

for val in ["nx"] + mesh_nx_options:
    save_to_worksheet(timings_sheet, next_excel_col, val)

if original_f90_file:
    correct_outputs = {}
    for nx in mesh_nx_options:
        print(f"Calculating correct output for nx = {nx}")
        change_nx_value(original_f90_file, mesh_nx_options, nx)
        output = get_correct_output(original_f90_file)
        correct_outputs[nx] = output
    
    change_nx_value(original_f90_file, mesh_nx_options, mesh_nx_options[0])
    print("Finshed getting correct outputs")

for cmd, option_name in compiler_cmds:

    if cmd.endswith(".bat"):
        compile_cmd = cmd
    else:
        compile_cmd = cmd + " -o output.exe " + f90_file

    next_excel_col += 1

    for nx_option in mesh_nx_options:
        # Change value of nx in fortran code and add new nx row to spreadsheet
        print(f"\nChanging nx value to {nx_option}")
        change_nx_value(f90_file, mesh_nx_options, nx_option)
        last_nx_val = nx_option

        if nx_option == mesh_nx_options[0]:
            save_to_worksheet(timings_sheet, next_excel_col, option_name)

        print("\nCompiling using: " + compile_cmd)
        subprocess.run(compile_cmd.split(), stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

        if os.path.exists("output.exe"):
            print("Successfully compiled. Running output.exe")
        else:
            print("Compile failed. Moving to next compiler command.")
            break

        if original_f90_file:
            if is_output_correct(correct_outputs[nx_option], "./output.exe"):
                print("Code output is CORRECT. Measuring runtime.")
            else:
                print("INCORRECT output file produced. Moving to next compiler command.")
                save_to_worksheet(timings_sheet, next_excel_col, "N/A")
                os.remove("output.exe")
                break

        elapsed_time = timeit(
            stmt = "subprocess.run('./output.exe', stdout=subprocess.DEVNULL)",
            setup = "import subprocess",
            number = reps) / reps

        print(f"Saving elapsed time {elapsed_time:3f} to worksheet")
        save_to_worksheet(timings_sheet, next_excel_col, elapsed_time)

        os.remove("output.exe")
    
    workbook.save(filename)

change_nx_value(f90_file, mesh_nx_options, mesh_nx_options[0])

print(f"\nCompleted test run. Saving worksheet to {filename}.")
workbook.save(filename)
