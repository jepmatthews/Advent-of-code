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
    if len(components) == 1:
        return components[0] == total
    if total % components[-1] == 0:
        return equation_is_valid(total/components[-1],components[0:-1])\
            or equation_is_valid(total - components[-1],components[0:-1])
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