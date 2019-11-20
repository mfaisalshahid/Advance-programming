const fs = require('fs')
const http = require('http')
const socketIO = require('socket.io')
pairs = []
count = 0

const readFile = f => new Promise((resolve,reject) =>
	fs.readFile(f, (e, d) =>{
		e?reject(e):resolve(d) 
		})
	)

const server = http.createServer(async (req,resp) =>{
		resp.end(await readFile(req.url.substr(1)))
		})

const io = socketIO(server)
io.sockets.on('connection' , socket =>{
		pairs.push(socket)
		// console.log(pairs.length)
		console.log(pairs.length)
		if(pairs.length%2 == 0)
		{
			pairs[pairs.length-1].emit('start', 'join')
			pairs[pairs.length-1].emit('sign', 'X')
			pairs[pairs.length-1].on('mark', data => 
			{
				// console.log(data)
				pairs[pairs.length-2].emit('markSign1',data)
				// pairs[pairs.length-2].emit('start','join')

			})
			pairs[pairs.length-1].on('result', data => 
			{
				// console.log(data)
				pairs[pairs.length-1].emit('result',data)	
				pairs[pairs.length-2].emit('result',data)
			})
			pairs[pairs.length-2].emit('start', 'join')
			pairs[pairs.length-2].emit('sign', '0')
			pairs[pairs.length-2].on('mark', data => 
			{
				// console.log(data)
				pairs[pairs.length-1].emit('markSign2',data)
				// pairs[pairs.length-1].emit('start','join')

			})
			pairs[pairs.length-2].on('result', data => 
			{
				// console.log(data)
				pairs[pairs.length-1].emit('result',data)	
				pairs[pairs.length-2].emit('result',data)
			})
			pairs[pairs.length-1].on('turn', data => 
			{
				// console.log(data)
				// data = 'true'
				pairs[pairs.length-2].emit('turn',data)
				// pairs[pairs.length-2].emit('start','join')

			})
			pairs[pairs.length-2].on('turn', data => 
			{
				// console.log(data)
				// data = 'true'
				pairs[pairs.length-1].emit('turn',data)
				// pairs[pairs.length-2].emit('start','join')

			})

		}
		else
		{
			pairs[pairs.length-1].emit('start' , 'wait')
		}
		
		
})
		// io.sockets.emit('yourmsg', data) }))

server.listen(3000 ,() => console.log('Started...'))

