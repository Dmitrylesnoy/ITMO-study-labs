# coding: utf-8

import xml.etree.ElementTree as xml  # XML документ в виде дерева
from datetime import datetime  # класс для работы с датами и временем
from enum import IntEnum, unique  # перечисления
from typing import Tuple  # аннотации типов для подсветки ошибок
from urllib.parse import unquote  # парсер url кодировки

from requests import get as http_get  # открытие ссылок
import colorama  # цветной вывод


class WeatherData:  # класс для работы с прогнозами для одного города
    class Forecast:  # класс для представления прогноза
        class Phenomena:  # класс для представления атмосферных явлений
            @unique  # проверка что у нас нет одинаковых литералов в перечислений
            class Cloudiness(IntEnum):  # перечисление для облачности
                FOG = -1
                CLEAR = 0
                LITTLE_CLOUDY = 1
                CLOUDY = 2
                MAINLY_CLOUDY = 3

            @unique
            class Precipitation(IntEnum):  # перечисление для осадков
                MIXED = 3
                RAIN = 4
                SHOWER = 5
                SNOW = 6
                STORM = 8
                UNKNOWN = 9
                CLEAR = 10

            def __init__(self, cloudiness: Cloudiness, precipitation: Precipitation, rpower: bool, spower: bool):
                # конструктор для инициализации полей, не рекомендуется для вызова извне
                self.cloudiness = cloudiness
                self.precipitation = precipitation
                self.rpower = rpower
                self.spower = spower

        class Pressure:  # класс для работы с давлением
            def __init__(self, min: float, max: float):
                # конструктор для инициализации полей, не рекомендуется для вызова извне
                self.min = min
                self.max = max

        class Temperature:  # класс для работы с температурой
            def __init__(self, min: float, max: float):
                # конструктор для инициализации полей, не рекомендуется для вызова извне
                self.min = min
                self.max = max

        class Wind:  # класс для работы с ветром
            @unique
            class Direction(IntEnum):  # перечисление с направлениями ветра
                NORTH = 1
                NORTH_EAST = 2
                EAST = 3
                SOUTH_EAST = 4
                SOUTH = 5
                SOUTH_WEST = 6
                WEST = 7
                NORTH_WEST = 8

            def __init__(self, min: float, max: float, direction: Direction):
                # конструктор для инициализации полей, не рекомендуется для вызова извне
                self.min = min
                self.max = max
                self.direction = direction

        class Relwet:  # класс для работы с относительной влажностью
            def __init__(self, min: float, max: float):
                # конструктор для инициализации полей, не рекомендуется для вызова извне
                self.min = min
                self.max = max

        class Heat:  # класс для работы с предпочтительной температурой
            def __init__(self, min: float, max: float):
                # конструктор для инициализации полей, не рекомендуется для вызова извне
                self.min = min
                self.max = max

        def __init__(
                self,
                date: datetime,
                predict: float,
                phenomena: Phenomena,
                pressure: Pressure,
                temperature: Temperature,
                wind: Wind,
                relwet: Relwet,
                heat: Heat
        ):
            # конструктор для инициализации полей, не рекомендуется для вызова извне
            self.date = date
            self.predict = predict
            self.phenomena = phenomena
            self.pressure = pressure
            self.temperature = temperature
            self.wind = wind
            self.relwet = relwet
            self.heat = heat

    def __init__(self, index: int, name: str, latitude: float, longitude: float, forecasts: Tuple[Forecast]):
        # конструктор для инициализации полей, не рекомендуется для вызова извне
        self.index = index
        self.name = name
        self.latitude = latitude
        self.longitude = longitude
        # делаем список прогнозов приватным, но делаем весь класс похожим на список чтобы не обращаться каждый раз к полю .forecasts
        self.__forecasts = forecasts  # поля начинающиеся с двух подчёркиваний считаются приватными и их нельзя вызвать извне класса (можно, но нужно приложить некоторые усилия)

    def __getitem__(self, i):
        # перегрузка оператора обращения по индексу []
        return self.__forecasts[i]

    def __len__(self):
        # перегрузка оператора длины чтобы пользоватся функцией len(...)
        return len(self.__forecasts)

    def __iter__(self):
        # перегрузка оператора итерирования чтобы использовать этот класс в цикле for ... in ... и в функции iter(...)
        return iter(self.__forecasts)

    @classmethod
    def from_xml(cls, raw_xml_data: str) -> 'WeatherData':
        # конструктор класса которые парсит строку с XML
        xml_data = xml.fromstring(raw_xml_data)  # Получение XML в виде дерева
        assert len(xml_data) == 1  # убеждаемся что у нас только один репорт, так как это не прописано в документации
        report = xml_data[0]
        assert len(report) == 1  # убеждаемся что у нас только один город, так как это не прописано в документации
        town = report[0]
        forecasts = []
        for forecast in town:
            children = {child.tag: child.attrib for child in forecast}  # представляем список потомков в виде словаря имя=аттрибуты и откидывая список потомков потому что нам их не должны передавать
            # сокращает конструкцию:
            # for tag in child:
            #     if tag.name == "...":
            #         ...
            #     elif tag.name == "...":
            #         ...
            #     ...

            # собираем объекты и конвертируем типы (парсер возвращает только строки)
            forecasts.append(
                cls.Forecast(
                    date=datetime(
                        year=int(forecast.attrib["year"]),
                        month=int(forecast.attrib["month"]),
                        day=int(forecast.attrib["day"]),
                        hour=int(forecast.attrib["hour"]),
                    ),
                    predict=float(forecast.attrib["predict"]),
                    phenomena=cls.Forecast.Phenomena(
                        cloudiness=cls.Forecast.Phenomena.Cloudiness(int(children["PHENOMENA"]["cloudiness"])),
                        precipitation=cls.Forecast.Phenomena.Precipitation(int(children["PHENOMENA"]["precipitation"])),
                        rpower=int(children["PHENOMENA"]["rpower"]) == 1,
                        spower=int(children["PHENOMENA"]["spower"]) == 1
                    ),
                    pressure=cls.Forecast.Pressure(
                        min=float(children["PRESSURE"]["min"]),
                        max=float(children["PRESSURE"]["max"]),
                    ),
                    temperature=cls.Forecast.Temperature(
                        min=float(children["TEMPERATURE"]["min"]),
                        max=float(children["TEMPERATURE"]["max"]),
                    ),
                    wind=cls.Forecast.Wind(
                        min=float(children["WIND"]["min"]),
                        max=float(children["WIND"]["max"]),
                        direction=cls.Forecast.Wind.Direction(int(children["WIND"]["direction"]))
                    ),
                    relwet=cls.Forecast.Relwet(
                        min=float(children["RELWET"]["min"]),
                        max=float(children["RELWET"]["max"]),
                    ),
                    heat=cls.Forecast.Heat(
                        min=float(children["HEAT"]["min"]),
                        max=float(children["HEAT"]["max"]),
                    )
                )
            )

        # собираем объект с прогнозами и данными о городе
        return cls(
            index=int(town.attrib["index"]),
            name=unquote(str(town.attrib["sname"])),  # парсим url-encoded название города
            latitude=float(town.attrib["latitude"]),
            longitude=float(town.attrib["longitude"]),
            forecasts=tuple(forecasts)
        )


# строковые представления для перечислений
cloudiness_texts = {
    WeatherData.Forecast.Phenomena.Cloudiness.FOG: "туман",
    WeatherData.Forecast.Phenomena.Cloudiness.CLEAR: "ясно",
    WeatherData.Forecast.Phenomena.Cloudiness.LITTLE_CLOUDY: "малооблачно",
    WeatherData.Forecast.Phenomena.Cloudiness.CLOUDY: "облачно",
    WeatherData.Forecast.Phenomena.Cloudiness.MAINLY_CLOUDY: "памурно",
}
precipitation_texts = {
    WeatherData.Forecast.Phenomena.Precipitation.MIXED: "смешанные",
    WeatherData.Forecast.Phenomena.Precipitation.RAIN: "дождь",
    WeatherData.Forecast.Phenomena.Precipitation.SHOWER: "ливень",
    WeatherData.Forecast.Phenomena.Precipitation.SNOW: "снег",
    WeatherData.Forecast.Phenomena.Precipitation.STORM: "гроза",
    WeatherData.Forecast.Phenomena.Precipitation.UNKNOWN: "нет данных",
    WeatherData.Forecast.Phenomena.Precipitation.CLEAR: "без осадков",
}
direction_texts = {
    WeatherData.Forecast.Wind.Direction.NORTH: "север",
    WeatherData.Forecast.Wind.Direction.NORTH_EAST: "северо-восток",
    WeatherData.Forecast.Wind.Direction.EAST: "восток",
    WeatherData.Forecast.Wind.Direction.SOUTH_EAST: "юго-восток",
    WeatherData.Forecast.Wind.Direction.SOUTH: "юг",
    WeatherData.Forecast.Wind.Direction.SOUTH_WEST: "юго-запад",
    WeatherData.Forecast.Wind.Direction.WEST: "запад",
    WeatherData.Forecast.Wind.Direction.NORTH_WEST: "северо-запад",
}

if __name__ == '__main__':  # условие чтоб запускать только если этот файл не импортируют из другого файла
    colorama.init()  # включаем поддержку цветного вывода

    # https://www.meteoservice.ru/content/export - сайт с погодой
    http_response = http_get('https://xml.meteoservice.ru/export/gismeteo/point/69.xml')  # открытие URL с XML погодой
    raw_data = http_response.text

    wd = WeatherData.from_xml(raw_data)  # вызываем конструктор который парсит XML

    # выводим
    # библиотека colorama позволяет вставлять в вывод последовательности которые система интерперетирует как цвета
    print(f"Город '{colorama.Back.CYAN}{wd.name}{colorama.Back.RESET}' ({colorama.Fore.LIGHTBLACK_EX}{wd.index}{colorama.Fore.RESET})")
    for fc in wd:
        print(f"  Прогноз на {colorama.Back.YELLOW}{fc.date.ctime()}{colorama.Back.RESET}")
        print(f"    Облачность: {colorama.Fore.CYAN}{cloudiness_texts[fc.phenomena.cloudiness]}{colorama.Fore.RESET} ")
        print(f"    Осадки: {colorama.Fore.BLUE}{precipitation_texts[fc.phenomena.precipitation]}{colorama.Fore.RESET} ")
        print(f"    Атмосферное давление: {colorama.Fore.YELLOW}{fc.pressure.min}{colorama.Fore.RESET}-{colorama.Fore.YELLOW}{fc.pressure.max}{colorama.Fore.RESET} мм.рт.ст")
        print(f"    Температура: {colorama.Fore.GREEN}{fc.temperature.min}{colorama.Fore.RESET}-{colorama.Fore.GREEN}{fc.temperature.max}{colorama.Fore.RESET} °C")
        print(f"    Ветер: {colorama.Fore.MAGENTA}{fc.wind.min}{colorama.Fore.RESET}-{colorama.Fore.MAGENTA}{fc.wind.max}{colorama.Fore.RESET} м/с на {colorama.Fore.MAGENTA}{direction_texts[fc.wind.direction]}{colorama.Fore.RESET}")
