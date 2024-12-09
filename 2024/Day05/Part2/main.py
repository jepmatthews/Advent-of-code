class RuleSet:
    def __init__(self,rules):
        self.rules = rules
    def __str__(self):
        return ", ".join(map(str,self.rules))
    def safe(self,pages):
        return all(map(lambda rule: rule.match(pages),self.rules))
    def correct(self,pages):
        while(not self.safe(pages)):
            for rule in self.rules:
                if(not rule.match(pages)):
                    x = pages[rule.first]
                    pages[rule.first] = pages[rule.second]
                    pages[rule.second] = x

class Rule:
    def __init__(self,first,second):
        self.first = first
        self.second = second
    def __str__(self):
        return f"{self.first}|{self.second}"
    def match(self, pages):
        if((self.first not in pages) or (self.second not in pages)):
            return True
        return pages[self.first] < pages [self.second]



f = open("../input.txt")

lines = f.readlines()
lines = list(map(lambda str: str.replace("\n",""),lines))
emptyLineIndex = lines.index("")
ruleLines = lines[0:emptyLineIndex]
pageLines = lines[emptyLineIndex+1:]

rules = []
for line in ruleLines:
    parts = line.split("|")
    rules.append(Rule(int(parts[0]),int(parts[1])))

ruleSet = RuleSet(rules)

updates = []
for line in pageLines:
    update = {}
    pages = line.split(",")
    for (index,page) in enumerate(pages):
        update[int(page)] = index
    updates.append(update)

result = 0

for update in filter(lambda update: not ruleSet.safe(update), updates):
    ruleSet.correct(update)
    numOfPages = len(list(enumerate(update)))
    for _,(page,index) in enumerate(update.items()):
        if index == int(numOfPages/2):
            result += page
    

print(result)