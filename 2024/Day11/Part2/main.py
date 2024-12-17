class StonesCalculator:
    def __init__(self):
        self.memo = {}
    def get_stones(self, stone, iterations):
        if((stone,iterations) in self.memo):
            return self.memo[(stone,iterations)]
        if(iterations == 0):
            self.memo[(stone,iterations)] = 1
            return 1
        if(stone == 0):
            value =  self.get_stones(1,iterations - 1)
            self.memo[(stone,iterations)] = value
            return value
        stoneString = str(stone)
        if(len(stoneString) %2 == 0):
            value = self.get_stones(int(stoneString[0:len(stoneString)//2]),iterations - 1) + self.get_stones(int(stoneString[len(stoneString)//2:]),iterations - 1) 
            self.memo[(stone,iterations)] = value
            return value
        else:
            value = self.get_stones(stone * 2024, iterations - 1) 
            self.memo[(stone,iterations)] = value
            return value



f = open("../input.txt")
stones = list(map(lambda i: int(i),f.readline().split()))

calculator = StonesCalculator()
result = 0

for stone in stones:
    result += calculator.get_stones(stone,75)


print(result)


