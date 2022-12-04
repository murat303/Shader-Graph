using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CollapsePosition : MonoBehaviour
{
    public Transform weaponPosition;
    public MeshRenderer _renderer;

    void Start()
    {
        
    }

    void Update()
    {
        _renderer.material.SetVector("_CollapsePoint", new Vector4(-weaponPosition.position.x, -weaponPosition.position.y, -weaponPosition.position.z, 0));
        weaponPosition.LookAt(transform);
    }
}
