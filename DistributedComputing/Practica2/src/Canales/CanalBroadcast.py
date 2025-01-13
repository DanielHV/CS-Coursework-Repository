import simpy
from Canales.Canal import Canal


class CanalBroadcast(Canal):
    '''
    Clase que modela un canal, permite enviar mensajes one-to-many.
    
    Atributos:
        env (simpy.Environment): El entorno de simulación de SimPy.
        capacidad (int): La capacidad del canal. Por defecto es infinito.
        canales (list): Lista de canales de entrada para los vecinos.
    '''

    def __init__(self, env, capacidad=simpy.core.Infinity):
        '''
        Inicializa el canal con el entorno de simulación y la capacidad.
        
        Args:
            env (simpy.Environment): El entorno de simulación de SimPy.
            capacidad (int, opcional): La capacidad del canal. Por defecto es infinito.
        '''
        self.env = env
        self.capacidad = capacidad
        self.canales = []

    def envia(self, mensaje, vecinos):
        '''
        Envía un mensaje a los canales de entrada de los vecinos.
        
        Args:
            mensaje (any): El mensaje a enviar.
            vecinos (list): Lista de índices de los vecinos a los que se enviará el mensaje.
        
        Comportamiento:
            - Envía el mensaje al canal de entrada de cada vecino.
        '''
        eventos = []
        for vecino in vecinos:
            evento = self.canales[vecino].put(mensaje)
            eventos.append(evento)

        return self.env.all_of(eventos)

    def crea_canal_de_entrada(self):
        '''
        Crea un canal de entrada.
        
        Returns:
            simpy.Store: Un nuevo canal de entrada con la capacidad especificada.
        
        Comportamiento:
            - Crea un nuevo canal de entrada y lo añade a la lista de canales.
            - Regresa el canal de entrada creado.
        '''
        canal_entrada = simpy.Store(self.env, capacity=self.capacidad)
        self.canales.append(canal_entrada)
        return canal_entrada

