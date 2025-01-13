/**
 * Lista doblemente ligada circular construida desde 0
 * Implementa genericos
 * 
 * @author Daniel Hernandez Vela
 * @version Practica 3
 */
public class Lista<T> {

    /** Primer y ultimo nodo de la lista */
    protected Nodo<T> primerNodo;
    protected Nodo<T> ultimoNodo;

    /**
     * Constructor que inicializa los nodos en null
     */
    public Lista(){
        primerNodo = ultimoNodo = null;
    }

    /**
     * Método que devuelve el primer nodo
     * 
     * @return Nodo<T> el primer nodo de la lista
     */
    public Nodo<T> getPrimerNodo(){
        return primerNodo;
    }

    /**
     * Método que devuelve el ultimo nodo
     * 
     * @return Nodo<T> el ultimo nodo de la lista
     */
    public Nodo<T> getUltimoNodo(){
        return ultimoNodo;
    }


    /**
     * Método que imprime la lista
     */
    public void imprimir(){
        Nodo<T> actual = primerNodo;
        if(primerNodo == null){
            System.out.println("La lista esta vacia");
        }else{
            while(actual != null){
                if(actual.getSiguiente() == primerNodo){
                    System.out.println(actual.getDatos());
                    break;
                }else{
                    System.out.print(actual.getDatos() + "  <->  ");
                    actual = actual.getSiguiente();
                }
                
            }
        }
    }

    /**
     * Método que imprime la lista en reversa
     */
    public void imprimirReversa(){
        Nodo<T> actual = ultimoNodo;
        if(primerNodo == null){
            System.out.println("La lista esta vacia");
        }else{
            while(actual != null){
                if(actual.getAnterior() == ultimoNodo){
                    System.out.println(actual.getDatos());
                    break;
                }else{
                    System.out.print(actual.getDatos() + "  <->  ");
                    actual = actual.getAnterior();
                }
                
            }
        }
    }


    /**
     * Método que agrega un objeto en la primera posicion de la lista
     * 
     * @param obj el objeto a agregar a la lista
     */
    public void push(T obj){
        Nodo<T> nuevo = new Nodo(obj);
        if(primerNodo == null){
            primerNodo = ultimoNodo = nuevo;
        }else{
            nuevo.setSiguiente(primerNodo);
            nuevo.setAnterior(ultimoNodo);
            primerNodo.setAnterior(nuevo);
            ultimoNodo.setSiguiente(nuevo);
            primerNodo = nuevo;
        }
    }


    /**
     * Método que agrega un objeto en la ultima posicion de la lista
     * 
     * @param obj el objeto a agregar a la lista
     */
    public void insertarAlFinal(T obj){
        Nodo<T> nuevo = new Nodo(obj);
        if(primerNodo == null){
            primerNodo = ultimoNodo = nuevo;
        }else{
            nuevo.setSiguiente(primerNodo);
            nuevo.setAnterior(ultimoNodo);
            primerNodo.setAnterior(nuevo);
            ultimoNodo.setSiguiente(nuevo);
            ultimoNodo = nuevo;
        }
    }


    /**
     * Método que elimina un elemento segun su posicion en la lista
     * 
     * @param indice el indice del elemento a eliminar
     */
    public void eliminarByIndice(int indice){

        if((indice >= 0) && (indice < this.getTamanio())){

            Nodo<T> actual = primerNodo;
            int i=0;

            if(primerNodo == null){
                System.out.println("La lista esta vacia");
            }else{
                while(actual != null){

                    if(this.getTamanio() == 1){
                        primerNodo = ultimoNodo = null;
                        break;
                    }

                    if(indice == i){
                        if(i == 0){
                            actual.getAnterior().setSiguiente(actual.getSiguiente());
                            actual.getSiguiente().setAnterior(actual.getAnterior());
                            primerNodo = actual.getSiguiente();
                            break;
                        }else if(i == (this.getTamanio()-1)){
                            actual.getAnterior().setSiguiente(actual.getSiguiente());
                            actual.getSiguiente().setAnterior(actual.getAnterior());
                            ultimoNodo = actual.getAnterior();
                        }else{
                            actual.getAnterior().setSiguiente(actual.getSiguiente());
                            actual.getSiguiente().setAnterior(actual.getAnterior());
                            break;
                        }

                    }else{
                        actual = actual.getSiguiente();
                        i++;
                    }
                }
            }

        }else{
            System.out.println("Numero invalido");
        }
  
    }
    

    /**
     * Método que agrega un objeto en la posicion deseada
     * 
     * @param obj el objeto a agregar a la lista
     * @param indice el indice donde se agregara el objeto
     */
    public void insertarByIndice(T obj, int indice){

        if((indice >= 0) && (indice < this.getTamanio())){
            
            Nodo<T> nuevo = new Nodo(obj);
            Nodo<T> actual = primerNodo;
            int i=0;

            if(primerNodo == null){
                System.out.println("La lista esta vacia");
            }else{
                while(actual != null){
                    if(indice == i){
                        if(i == 0){
                            nuevo.setSiguiente(actual);
                            nuevo.setAnterior(actual.getAnterior());
                            actual.getAnterior().setSiguiente(nuevo);
                            actual.setAnterior(nuevo);
                            primerNodo = nuevo;
                            break;
                        }else if(i == (this.getTamanio()-1)){
                            nuevo.setSiguiente(actual);
                            nuevo.setAnterior(actual.getAnterior());
                            actual.getAnterior().setSiguiente(nuevo);
                            actual.setAnterior(nuevo);
                            ultimoNodo = nuevo.getSiguiente();
                            break;
                        }else{
                            nuevo.setSiguiente(actual);
                            nuevo.setAnterior(actual.getAnterior());
                            actual.getAnterior().setSiguiente(nuevo);
                            actual.setAnterior(nuevo);
                            break;
                        }

                    }else{
                        actual = actual.getSiguiente();
                        i++;
                    }

                }
            }

        }else{
            System.out.println("Numero invalido");
        }

    }


    /**
     * Método que devuelve el tamanio de la lista
     * 
     * @return int el tamanio de la lista
     */
    public int getTamanio(){
        Nodo<T> actual = primerNodo;
        int tamanio = 0;
        if(primerNodo == null){
            return 0;
        }else{
            while(actual != null){
                tamanio++;
                if(actual.getSiguiente() == primerNodo){
                    break;
                }else{
                    actual = actual.getSiguiente();
                }
                
            }
            return tamanio;
        }
    }


    /**
     * Método que devuelve elemento en una posicion especifica
     * 
     * @param indice el indice en el que se desea obtener el elemento
     * @return T el elemento buscado
     */
    public T getElementoByIndice(int indice){
        Nodo<T> actual = primerNodo;
        if(indice < 0){
            throw new IndexOutOfBoundsException();
        }
        if(indice >= getTamanio()){
            throw new IndexOutOfBoundsException();
        }
 
        for (int i = 0; i < indice; i++){
            actual = actual.getSiguiente();
        }
        return actual.getDatos();
    }


    /**
     * Método que devuelve el elemento en el primer nodo de la lista
     * 
     * @return T el primer elemento
     */
    public T getPrimerElemento(){
        return primerNodo.getDatos();
    }


    /**
     * Método que devuelve el elemento en el ultimo nodo de la lista
     *
     * @return T el ultimo elemento
     */
    public T getUltimoElemento(){
        return ultimoNodo.getDatos();
    }


    /**
     * Método que busca un objeto en la lista
     * 
     * @param obj el objeto a buscar
     * @return boolean true si se encontro el objeto, false si no
     */
    public boolean busca(T obj){
        Nodo actual = primerNodo;
        boolean encontrado = false;
        while(actual.getSiguiente() != primerNodo){
            if(actual.getDatos().equals(obj)){
                encontrado = true;
                break;
            }else{
                actual = actual.getSiguiente();
            }
        }
        return encontrado;
    }


    /**
     * Método que devuelve el indice de un objeto especifico
     * 
     * @param obj el elemento del que se desea conocer su indice
     * @return int el indice del elemento si se encontro, -1 si no se encontro
     */
    public int getIndiceByElemento(T obj){
        Nodo actual = primerNodo;
        int i = 0;
        boolean encontrado = false;
        while(actual.getSiguiente() != primerNodo){
            if(actual.getDatos().equals(obj)){
                encontrado = true;
                break;
            }else{
                actual = actual.getSiguiente();
                i++;
            }
        }
        if(encontrado){
            return i;
        }else{
            return -1;
        }
    }


    /**
     * Método que comprueba si la lista esta vacia
     * 
     * @return boolean si la lista esta vacia true, si no false
     */
    public boolean estaVacia(){
        if(primerNodo == null){
            return true;
        }else{
            return false;
        }
    }

}