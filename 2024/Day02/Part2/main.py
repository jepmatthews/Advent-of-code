def report_is_safe(report):
    diffs = list(map(lambda a,b: int(b) - int(a), report[0:-1],report[1:]))
    if diffs_are_safe(diffs):
        return True
    if diffs_are_safe(diffs[1:]):
        return True
    if diffs_are_safe(diffs[0:-1]):
        return True
    for (index,diff) in list(enumerate(diffs))[0:-1]: 
        if diffs_are_safe(diffs[0:index] + [diff + diffs[index+1]] + diffs[index+2:]):
            return True
    if diffs_are_safe(diffs[0:index] + [diff + diffs[index+1]]):
        return True 
    return False

def diffs_are_safe(diffs):
    return all(map(lambda d: 0 < d <= 3, diffs)) or all(map(lambda d: 0 < -d <= 3, diffs))

f = open("../input.txt")
print(list((map(lambda lin: report_is_safe(lin.split()),f.readlines()))).count(True))