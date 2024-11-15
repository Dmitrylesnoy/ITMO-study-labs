import re
import json
import time

xml_str = open("schedule.xml").read().replace("\n", "")


def convertingXML2JSON(xml):
    """
    Convert object contain from XML to JSON
    """
    pattern = r"<([^/>]+)>(.*?)</\1>"
    obj = {}

    matches = re.findall(pattern, xml)

    for header, content in matches:
        inner_matches = re.findall(
            pattern, content
        )  # Проверяем, есть ли  обхекты и хар-ки
        if inner_matches:
            inner_cont = convertingXML2JSON(
                content
            )  # Если есть вложенные обхекты, рекурсивно обрабатываем
        else:
            inner_cont = content.strip()

        # Если обхект уже существует в нашей памяти, добавляем к нему
        if header in obj:
            if isinstance(obj[header], list):
                obj[header].append(inner_cont)
            else:
                obj[header] = [obj[header], inner_cont]
        else:
            obj[header] = inner_cont  # Сохраняем первый объект

    return obj


timer = time.time()
with open("schedule.json", "w") as file:
    json.dump(convertingXML2JSON(xml_str), file, indent=4)
    timer_ = time.time()
    print((timer_ - timer) * 1000)
