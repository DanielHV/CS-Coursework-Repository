import simpy
from Nodo import *
from Canales.CanalBroadcast import *

TICK = 1

class NodoGenerador(Nodo):
    ''' 
    Implementa la interfaz de Nodo para el algoritmo de flooding.
    
    Atributos:
        id_nodo (int): Identificador único del nodo.
        vecinos (list): Lista de nodos vecinos.
        canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
        canal_salida (CanalBroadcast): Canal de salida para enviar mensajes.
        padre (int): Identificador del nodo padre en el árbol generador.
        hijos (list): Lista de identificadores de los nodos hijos en el árbol generador.
        mensajes_esperados (int): Cantidad de mensajes que se esperan recibir.
    '''
    def __init__(self, id_nodo, vecinos, canal_entrada, canal_salida):
        '''
        Inicializa el nodo.
        
        Args:
            id_nodo (int): Identificador único del nodo.
            vecinos (list): Lista de nodos vecinos.
            canal_entrada (simpy.Store): Canal de entrada para recibir mensajes.
            canal_salida (CanalBroadcast): Canal de salida para enviar mensajes.
        '''
        self.id_nodo = id_nodo
        self.vecinos = vecinos
        self.canal_entrada = canal_entrada
        self.canal_salida = canal_salida
        
        # Atributos propios del algoritmo
        self.padre = None if id_nodo != 0 else id_nodo # Si es el nodo distinguido, el padre es el mismo 
        self.hijos = list()
        self.mensajes_esperados = len(vecinos) # Cantidad de mensajes que esperamos
    
    def genera_arbol(self, env):
        '''
        Algoritmo que construye un árbol generador
        
        Args:
            env (simpy.Environment): El entorno de simulación de SimPy.
        
        Comportamiento:
            - Si el nodo es el nodo distinguido (id_nodo == 0), envía un mensaje "GO" a todos sus vecinos.
            - Cuando un nodo recibe un mensaje "GO" y no tiene padre, se asigna el nodo que envió el mensaje como su padre y reenvía el mensaje "GO" a sus vecinos (excepto al padre).
            - Cuando un nodo recibe un mensaje "GO" pero ya tiene un padre asignado, envía de vuelta un mensaje "BACK" sin identificador al nodo que envió "GO".
            - Cuando un nodo ha recibido todos los mensajes esperados, envía un mensaje "BACK" a su padre.
            - Cuando un nodo recibe un mensaje "BACK", decrece su contador de mensajes esperados, y asigna al nodo que envió "BACK" como hijo.
            - Cuando un nodo recibe un mensaje  "BACK" sin identificador, decrece su contador pero no asigna ningún hijo.
        '''
        
        if self.id_nodo == 0:
            # enviar ("GO", id) a vecinos
            self.canal_salida.envia(("GO", self.id_nodo), self.vecinos)
        
        while True:
            mensaje = yield self.canal_entrada.get()
            
            # caso mensaje GO
            if mensaje[0] == "GO":
                if self.padre == None:
                    self.padre = mensaje[1]
                    self.mensajes_esperados = len(self.vecinos) - 1
                    if self.mensajes_esperados == 0:
                        # enviar ("BACK", id) al nodo que envió "GO"
                        self.canal_salida.envia(("BACK", self.id_nodo), [mensaje[1]])
                    else:
                        # enviar ("GO", id) a vecinos menos al padre
                        vecinos_a_enviar = self.vecinos.copy()
                        vecinos_a_enviar.remove(self.padre)
                        self.canal_salida.envia(("GO", self.id_nodo), vecinos_a_enviar)
                else:
                    # enviar ("BACK", None) al nodo que envió "GO"
                    self.canal_salida.envia(("BACK", None), [mensaje[1]])
            
            # caso mensaje BACK
            elif mensaje[0] == "BACK":
                self.mensajes_esperados -= 1
                if mensaje[1] != None:
                    self.hijos.append(mensaje[1])
                if self.mensajes_esperados == 0:
                    if self.padre != self.id_nodo:
                        # enviar ("BACK", id) al padre
                        self.canal_salida.envia(("BACK", self.id_nodo), [self.padre])
                        