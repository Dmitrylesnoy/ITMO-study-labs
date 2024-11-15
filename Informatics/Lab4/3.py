import re
import json
import time

xml_str = open("schedule.xml").read().replace("\n", "")


def convertingXML2JSON(xml):
    """
    Convert object contain from XML to JSON
    """
    pattern = r"<([^/>]+)>(.*?)</\1>"
    json_obj = {}

    matches = re.findall(pattern, xml)

    for tag, content in matches:
        if content:
            # Проверяем, есть ли вложенные обхекты и хар-ки
            inner_matches = re.findall(pattern, content)
            if inner_matches:
                json_content = convertingXML2JSON(
                    content
                )  # Если есть вложенные обхекты, рекурсивно обрабатываем
            else:
                json_content = content.strip()

            # Если обхект уже существует в нашей памяти, добавляем к нему
            if tag in json_obj:
                if isinstance(json_obj[tag], list):
                    json_obj[tag].append(json_content)
                else:
                    json_obj[tag] = [json_obj[tag], json_content]
            else:
                json_obj[tag] = json_content  # Сохраняем первый объект
        else:
            json_obj[tag] = None  # Если содержимое пустое, устанавливаем значение None

    return json_obj


timer = time.time()
with open("schedule.json", "w", encoding="utf-8") as f:
    json.dump(convertingXML2JSON(xml_str), f, ensure_ascii=False, indent=4)
    timer_ = time.time()
    print((timer_ - timer) * 1000)
