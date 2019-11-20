// const url = 'http://api.openweather.org/data/2.5/weather?units=metric&appid=<YOUR-APPID-HERE>'
let temp = 0
let city = ''

const formSubmit = async ev => {
	ev.preventDefault()
	temp = temp+1
	socket.emit('mymsg' , 'okay')
	redraw()
}

const redraw = () =>
	ReactDOM.render(
		React.createElement('submit', {
			type: 'submit',
			value: 'gun'			
		}),
		React.createElement('h2' , null, `T: ${temp}`),
		// React.createElement('form',{onSubmit: formSubmit},
		// 	React.createElement('input' , {
		// 	type: 'text',
		// 	// onChange: ev => city = ev.target.value
		// 	}),
		// 	React.createElement('input',{type: 'submit'}),
		// 	React.createElement('h2' , null, `T: ${temp}`)),
		document.getElementById('root'))



redraw()