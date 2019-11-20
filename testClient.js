setState = {}
gameBoard = [['-','-','-','-','-','-'],['-','-','-','-','-','-'],['-','-','-','-','-','-'],['-','-','-','-','-','-'],['-','-','-','-','-','-'],['-','-','-','-','-','-'],['-','-','-','-','-','-']]
var dimensions = [gameBoard.length,gameBoard[0].length ];
console.log(dimensions)
temp = [6,6,6,6,6,6,6]
clicks = 0
const socket = io()
socketWinner = ''
signData = ''
turnCheck = true
// const socket = io()
console.log('socket',socket)
idcount = 0 
colorCheck = 0
arrayReduce = 1
const state = updates =>
{
	Object.assign(setState,updates)
	if(setState.current == 2)
		Root = map
	if(setState.current == 1)
		Root = waitScreen
	if(setState.current == 3)
		Root = winningCondition
	if(setState.current == 4)
		Root = finalScreen
	ReactDOM.render(React.createElement(Root,setState),document.getElementById('root'))
}


const map = () => 
{
	return React.createElement('gameBoard',null,
	gameBoard.map(row=> {
		return React.createElement('div',null,
			row.map(col => {
				return React.createElement('input',{
					type : 'submit',
					id : idcount++,
					value : ' ',
					className: 'button',
					onMouseOver : ev => 
					{
						// if()a
						let t = (ev.target.id%6)
						for(i = 0 ; i < 7 ; i++)
						{	
							document.getElementById(t).style = 'background-color : brown';
							t = t+6
						}
					},
					onMouseOut : ev =>
					{
						// if(colorCheck == 0)
						// {	
							let t = (ev.target.id%6)
							for(i = 0 ; i < 7 ; i++)
							{
								document.getElementById(t).style = 'background-color : grey';
								t = t+6	
							}
					},
					onClick : ev => 
					{
							let t = (ev.target.id%6)
							for(i = 0 ; i < 7 ; i++)
							{
								if (i == temp[ev.target.id%6])
								{	
									document.getElementById(t).style = 'background-color : green';
									if(colorCheck == 0 && turnCheck == true)
										{
										document.getElementById(t).value = signData
										// colorCheck = 1
										// turncheck = 'false'
										// turnCheck = false
										// socket.emit('turn',turncheck)
										socket.emit('mark' , t)
										temp[ev.target.id%6] = temp[ev.target.id%6] - arrayReduce
										// arrayReduce++
										}
										
									else
									{
										if(turnCheck == true){
										document.getElementById(t).value = signData
										// turncheck = 'false'
										// turnCheck = false
										// socket.emit('turn',turncheck)
										socket.emit('mark' , t)
										// colorCheck = 0
										
										temp[ev.target.id%6] = temp[ev.target.id%6] - arrayReduce
										// arrayReduce++
										}
									}
								
								winningCondition(t)

								}
								t = t+6	
							}
						
					}
				}
				)
			}))
	})
	)
}


socket.on('start', data =>{
	if(data == 'join')
	{
		// turnCheck = true
		state({current: '2'})
	}
	else if(data == 'wait')
	{
		state({current: '1'})
	}
})

socket.on('sign', data =>{

	if(data == 'X')
	{
		signData = data
		colorCheck = 0
	}
	else
	{
		signData = data
		colorCheck = 1
	}
})


// socket.on('turn', data =>{
// 	turnCheck = true
// 	// state({current: '2'})
// })


socket.on('markSign1', data =>{
	// turnCheck = true
	document.getElementById(data).value = 'X'
	temp[data%6] = temp[data%6] - arrayReduce
	// socket.emit('mark' , t)

	// arrayReduce++

})

socket.on('markSign2', data =>{
	// turnCheck = true
	document.getElementById(data).value = '0'
	temp[data%6] = temp[data%6] - arrayReduce
	// arrayReduce++
	// socket.emit('mark' , t)	

})



const waitScreen = () => 
{
	return React.createElement('h2', null, 'Please wait for the second player to join :)')
}

const winningCondition = (i) =>
{
	// console.log(i)
	if(document.getElementById(i+18) != null)
	{
			console.log('first')

			if((document.getElementById(i).value == document.getElementById(i+6).value) && (document.getElementById(i+6).value == document.getElementById(i+12).value) && (document.getElementById(i+12).value == document.getElementById(i+18).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}
	if(document.getElementById(i+21) != null)
	{

		if((document.getElementById(i).value == document.getElementById(i+7).value) && (document.getElementById(i+7).value == document.getElementById(i+14).value) && (document.getElementById(i+14).value == document.getElementById(i+21).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i-21) != null)
	{

		if((document.getElementById(i).value == document.getElementById(i-7).value) && (document.getElementById(i-7).value == document.getElementById(i-14).value) && (document.getElementById(i-14).value == document.getElementById(i-21).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i-15) != null)
	{

		if((document.getElementById(i).value == document.getElementById(i-5).value) && (document.getElementById(i-5).value == document.getElementById(i-10).value) && (document.getElementById(i-10).value == document.getElementById(i-15).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i+15) != null)
	{

		if((document.getElementById(i).value == document.getElementById(i+5).value) && (document.getElementById(i+5).value == document.getElementById(i+10).value) && (document.getElementById(i+10).value == document.getElementById(i+15).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i-1) != null && document.getElementById(i+2) != null)
	{

		if((document.getElementById(i).value == document.getElementById(i-1).value) && (document.getElementById(i).value == document.getElementById(i+1).value) && (document.getElementById(i).value == document.getElementById(i+2).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i+1) != null && document.getElementById(i-2) != null)
	{


		if((document.getElementById(i).value == document.getElementById(i-1).value) && (document.getElementById(i).value == document.getElementById(i+1).value) && (document.getElementById(i+1).value == document.getElementById(i-2).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i-10) != null && document.getElementById(i+5) != null)
	{
		// console.log('first')

		if((document.getElementById(i).value == document.getElementById(i+5).value) && (document.getElementById(i).value == document.getElementById(i-5).value) && (document.getElementById(i).value == document.getElementById(i-10).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i+10) != null && document.getElementById(i-5) != null)
	{

		if((document.getElementById(i).value == document.getElementById(i+5).value) && (document.getElementById(i).value == document.getElementById(i-5).value) && (document.getElementById(i).value == document.getElementById(i+10).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i-14) != null && document.getElementById(i+7) != null)
	{
		// console.log('first')

		if((document.getElementById(i).value == document.getElementById(i+7).value) && (document.getElementById(i).value == document.getElementById(i-7).value) && (document.getElementById(i).value == document.getElementById(i-14).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}

	if(document.getElementById(i-7) != null && document.getElementById(i+14) != null)
	{
		// console.log('first')

		if((document.getElementById(i).value == document.getElementById(i-7).value) && (document.getElementById(i).value == document.getElementById(i+7).value) && (document.getElementById(i).value == document.getElementById(i+14).value))
			{	
				socket.emit('result',socket.id)
				
			}
	}



}


socket.on('result', data =>{
	
	
	socketWinner = data
	state({current: '4'})
})

const finalScreen  = () =>
{
	return React.createElement('h2', null , 'Socker Player "', socketWinner,'" wins !!! Congratulations')

}

