class Equation:
    def __init__(self,line):
        parts = line.split()
        self.components = list(map(int,parts[1:]))
        self.total = int(parts[0][:-1])
    def __str__(self):
        componentString = " ".join(map(str,self.components))
        return f"{self.total}: {componentString}"
    def has_solution(self):
        return equation_is_valid(self.total, self.components)
    

def equation_is_valid(total,components):
    if(total < 0):
        return False
    if len(components) == 1:
        return components[0] == total
    elif total % components[-1] == 0 and equation_is_valid(int(total/components[-1]),components[0:-1]):
        return True
    elif len(components) >= 2 and str(total)[1:].endswith(str(components[-1])) \
            and equation_is_valid(int(str(total)[0:-(len(str(components[-1])))]),components[0:-1]):
        return True
    else:
        return equation_is_valid(total - components[-1],components[0:-1])

f = open("../input.txt")

equations = list(map(Equation,f.readlines()))

result = 0

for equation in equations:
    if(equation.has_solution()):
        result += equation.total
    print(f"{str(equation)} is valid: {equation.has_solution()}")

print(result)