package attention.training;

import attention.graphics.SignView;

public class SignWithPosition {
	private SignView sign;
	private Position position;
	
	public SignWithPosition(SignView sign, Position position) {
		this.sign = sign;
		this.position = position;
	}
	
	public SignView getSign() {
		return sign;
	}
	
	public Position getPosition() {
		return position;
	}
}
