import requests
from datetime import date

def get_url(year):
    return f"https://nolaborables.com.ar/api/v2/feriados/{year}"

months = ['Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre']
days = ['Lunes', 'Martes', 'Miércoles', 'Jueves', 'Viernes', 'Sábado', 'Domingo']

def day_of_week(day, month, year):
    return days[date(year, month, day).weekday()]

class NextHoliday:
    def __init__(self):
        self.loading = True
        self.year = date.today().year
        self.holiday = None
        self.holiday_type = None  # Nuevo atributo para almacenar el tipo de feriado

    def set_next(self, holidays):
        now = date.today()
        today = {
            'day': now.day,
            'month': now.month
        }
        
        # Considera el tipo de feriado en el filtrado
        filtered_holidays = [h for h in holidays if h['tipo'] == self.holiday_type] if self.holiday_type else holidays

        holiday = next(
            (h for h in filtered_holidays if h['mes'] == today['month'] and h['dia'] > today['day'] or h['mes'] > today['month']),
            filtered_holidays[0] if filtered_holidays else None  # Evita errores si no hay feriados que coincidan
        )

        self.loading = False
        self.holiday = holiday

    def fetch_holidays(self, holiday_type=None):
        self.holiday_type = holiday_type  # Almacena el tipo de feriado
        response = requests.get(get_url(self.year))
        if response.ok:
            data = response.json()
            self.set_next(data)
        else:
            print("Error al obtener los datos")

    def render(self):
        if self.loading:
            print("Buscando...")
        elif not self.holiday:
            print("No se encontraron feriados que coincidan con los criterios.")
        else:
            print(f"Próximo feriado: {self.holiday['motivo']}")
            print(f"Fecha: {day_of_week(self.holiday['dia'], self.holiday['mes'], self.year)} {self.holiday['dia']} de {months[self.holiday['mes'] - 1]}")
            print(f"Tipo: {self.holiday['tipo']}")

next_holiday = NextHoliday()
# next_holiday.fetch_holidays()
# next_holiday.fetch_holidays(holiday_type="puente")
next_holiday.render()
