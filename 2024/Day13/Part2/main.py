import re
from fractions import Fraction

class Vector:
    def __init__(self,dx,dy):
        self.dx = dx
        self.dy = dy
    def __add__(self,other):
        return Vector(self.dx + other.dx,self.dy + other.dy)
    def __mul__(self,other):
        if type(other) is int:
            return Vector(self.dx * other, self.dy * other)
        else:
            return NotImplemented
    def __sub__(self,other):
        return Vector(self.dx - other.dx,self.dy - other.dy)
    def __floordiv__(self,other):
        if(not self.is_integer_multiple_of(other)):
            return None
        return self.dy // other.dy
    def __str__(self):
        return f"dx:{self.dx}, dy:{self.dy}"
    def is_integer_multiple_of(self,other):
        return self.dx % other.dx == 0 and self.dy % other.dy == 0 and self.dx // other.dx == self.dy // other.dy
    def gradient(self) -> Fraction:
        return Fraction(self.dy,self.dx)


class Machine:
    def __init__(self,buttonA,buttonB,prize):
        self.buttonA = buttonA
        self.buttonB = buttonB
        self.prize = prize

def find_optimal_cost(machine):
    gradientA = machine.buttonA.gradient()
    gradientB = machine.buttonB.gradient()
    gradientPrize = machine.prize.gradient()
    if(gradientA == gradientPrize and gradientB == gradientPrize):
        if(machine.buttonA.dy > machine.buttonB.dy * 3):
            buttonBPresses=0
            while(buttonBPresses * machine.buttonB.dy < machine.prize.dy):
                vectorB = machine.buttonB * buttonBPresses
                if(machine.prize - vectorB).is_integer_multiple_of(machine.buttonA):
                    return buttonBPresses + (((machine.prize-vectorB) // machine.buttonA) * 3)
                buttonBPresses += 1
        else:
            buttonAPresses=0
            while(buttonAPresses * machine.buttonA.dy < machine.prize.dy):
                vectorA = machine.buttonA * buttonAPresses
                if(machine.prize - vectorA).is_integer_multiple_of(machine.buttonB):
                    return (buttonAPresses * 3) + ((machine.prize-vectorB) // machine.buttonA)
                buttonAPresses += 1
        return None
    elif(gradientA == gradientPrize):
        if(machine.prize.is_integer_multiple_of(machine.buttonA)):
            return 3 * (machine.prize // machine.buttonA)
        else:
            return None
    elif(gradientB == gradientPrize and machine.prize.is_integer_multiple_of(machine.buttonB)):
        if(machine.prize.is_integer_multiple_of(machine.buttonA)):
            return machine.prize // machine.buttonB
        else:
            return None
    else:
        buttonAPresses = Fraction((machine.prize.dx * machine.buttonB.dy) - (machine.prize.dy * machine.buttonB.dx),(machine.buttonA.dx * machine.buttonB.dy) - (machine.buttonA.dy*machine.buttonB.dx)) 
        buttonBPresses = Fraction(machine.prize.dx - (buttonAPresses*machine.buttonA.dx),machine.buttonB.dx)
        (aNumerator,aDenominator) = buttonAPresses.as_integer_ratio()
        (bNumerator,bDenominator) = buttonBPresses.as_integer_ratio()
        if(aDenominator == 1 and bDenominator == 1 and aNumerator > 0 and bNumerator > 0):
            return (aNumerator * 3) + bNumerator 
        else:
            return None

f = open("../input.txt")
regexMatchString = r"A: X\+(\d+).*Y\+(\d+)\n.+B: X\+(\d+).*Y\+(\d+)\nPrize: X=(\d+).+Y=(\d+)"

matches = re.findall(regexMatchString,f.read())

machines = []

for match in matches:
    machines.append(Machine(Vector(int(match[0]),int(match[1])),Vector(int(match[2]),int(match[3])),Vector(int(match[4])+10000000000000,int(match[5])+10000000000000)))

result = sum(filter(lambda cost: cost != None,map(lambda m: find_optimal_cost(m),machines)))

print(result)